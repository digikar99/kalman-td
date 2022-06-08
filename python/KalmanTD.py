# Implementation based on Gershman (2015)
#   "A Unifying Probabilistic View of Associative Learning"
# and Algorithm 1 from Geist and Pietquin (2010)
#   "Kalman Temporal Differences"

from numpy import *

# Keep methods separate from classes with Julia and Common Lisp style.
# Aids rapid prototyping while using emacs integrated REPL, since we
# do not need to reconstructKalmanTD instances.
class KalmanTD:
    # We are assuming num_reward_features = 1, otherwise calculation
    # for the Kalman Gain in the update function below do not seem to
    # make sense in terms of matrix dimensions
    def __init__(self, num_stimulus_features, tau, gamma, sigma_w, sigma_r):
        """
        tau     : (tau^2 * I) is added to the weights_cov at every time step
        gamma   : used to compute discounted_time_derivative
                  stimulus_features_old - gamma * stimulus_features_new
        sigma_w : initially, weights_cov = sigma_w^2 * identity_matrix
        sigma_r : actual_reward ~ normal(expected_noise, sigma_r)
        """
        self.weights_mean = zeros((num_stimulus_features, 1))
        self.num_weights  = num_stimulus_features
        self.weights_cov  = sigma_w**2 * identity(self.num_weights)
        self.tau          = tau
        self.weights_cov_noise = tau**2 * identity(self.num_weights)
        self.gamma = gamma
        self.sigma_w      = sigma_w
        self.sigma_r      = sigma_r
        self.old_kalman_gain  = None

def _compute_discounted_time_derivative(
        ktd:KalmanTD, stimulus_features_new, stimulus_features_old
):
    # Ht = Xt-Gamma*(Xt+1) : Correction Paper
    return stimulus_features_old - ktd.gamma * stimulus_features_new

def predict_reward(ktd:KalmanTD, stimulus_features_new, stimulus_features_old):
    # First compute discounted time derivative (h_t in the paper)
    discounted_time_derivative = _compute_discounted_time_derivative(
        ktd, stimulus_features_new, stimulus_features_old
    )

    # Then, check for correct dimensionality
    Ns1, _ = ktd.weights_mean.shape
    Ns2, _ = discounted_time_derivative.shape
    assert Ns1 == Ns2, \
        "Weights and stimulus are of incompatible shape: " + \
        "weights {0}, stimulus {1}".format(
            ktd.weights_mean.shape, discounted_time_derivative.shape
        )

    # Update the mean and covariance
    ktd.weights_mean = ktd.weights_mean # mean remains the same
    ktd.weights_cov  += ktd.weights_cov_noise

    # Algorithm 1 from Geist and Pietquin suggests that the predicted reward
    # is the expected reward over the weights
    return (ktd.weights_mean.T @ discounted_time_derivative).flat[0]

def update(
        ktd:KalmanTD,
        expected_reward,
        actual_reward,
        stimulus_features_new,
        stimulus_features_old
):
    """
    Update the weights, given the actual reward. We are taking
    expected_reward as an argument, because the weights_cov change in
    the mere process of calculating it.
    """

    discounted_time_derivative = _compute_discounted_time_derivative(
        ktd, stimulus_features_new, stimulus_features_old
    )
    delta = actual_reward - expected_reward

    # numer: vector of dimensions (num_stimulus_features, 1)
    numer = ktd.weights_cov @ discounted_time_derivative
    # denom: scalar
    denom = (discounted_time_derivative.T @ ktd.weights_cov \
             @ discounted_time_derivative).flat[0] \
             + ktd.sigma_r**2
    kalman_gain = numer / denom

    ktd.weights_mean += kalman_gain * delta
    ktd.weights_cov  -= kalman_gain @ discounted_time_derivative.T @ ktd.weights_cov
    # current kalman gain can only be computed after a round of predict
    ktd.old_kalman_gain = kalman_gain

    return

def default_kalman_td(num_stimulus_features):
    return KalmanTD(
        num_stimulus_features,
        tau=0.1,
        gamma=0.98,
        sigma_r=1,
        sigma_w=1
    )

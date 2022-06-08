from KalmanTD import *
import numpy as np
import matplotlib.pyplot as plt
from simulation_helper import *

def simulate_latent_inhibition():

	control_ktd      = default_kalman_td(1)
	experimental_ktd = default_kalman_td(1)

	preex_stimuli, preex_rewards = prepare_stimuli_rewards("A", 10, "A > -")
	process_stimuli_rewards(experimental_ktd, preex_stimuli, preex_rewards)

	exp_stimuli, exp_rewards = prepare_stimuli_rewards("A", 10, "A > +")
	ctrl_expected_rewards = \
		process_stimuli_rewards(
			control_ktd,
			exp_stimuli,
			exp_rewards,
			lambda ktd, exp_reward: exp_reward
		)
	exp_expected_rewards = \
		process_stimuli_rewards(
			experimental_ktd,
			exp_stimuli,
			exp_rewards,
			lambda ktd, exp_reward: exp_reward
		)

	plt.plot(arange(1,11), exp_expected_rewards[::2], label="Pre Group")
	plt.plot(arange(1,11), ctrl_expected_rewards[::2], label="No-pre Group")
	plt.title("Latent Inhibition aka Preexposure Effect")
	plt.xlabel("Stimulus")
	plt.ylabel("Expected Reward")
	plt.legend()
	plt.show()

	return exp_expected_rewards, ctrl_expected_rewards

def simulate_second_order_extinction():
	"""
	Phase 1: Z -> X -> - / X -> +
	Phase 2: X -> - for experimental, none for control
	"""

	control_ktd      = default_kalman_td(2)
	experimental_ktd = default_kalman_td(2)

	p1_stimuli, p1_rewards = prepare_stimuli_rewards("ZX", 10, "Z > X > - / X > +")
	p2_stimuli, p2_rewards = prepare_stimuli_rewards("ZX", 10, "X > -")
	test_stimuli, _ = prepare_stimuli_rewards("ZX", 1, "Z > -")

	process_stimuli_rewards(control_ktd, p1_stimuli, p1_rewards)
	process_stimuli_rewards(experimental_ktd, p1_stimuli, p1_rewards)

	process_stimuli_rewards(experimental_ktd, p2_stimuli, p2_rewards)

	exp_test_reward = predict_reward(
		experimental_ktd,
		test_stimuli[1],
		test_stimuli[0]
	)
	ctrl_test_reward = predict_reward(
		control_ktd,
		test_stimuli[1],
		test_stimuli[0]
	)

	barplot = plt.bar([1,2], [exp_test_reward, ctrl_test_reward])
	plt.xticks([1,2], ["Ext", "No-Ext"])
	plt.ylabel("Value")
	plt.title("Second Order Extinction")
	plt.legend()
	plt.show()

def simulate_serial_compound_latent_inhibition():
	"""
	Phase 1: X -> - for experimental; none for control
	Phase 2: Z -> X -> +
	"""
	control_ktd      = default_kalman_td(2)
	experimental_ktd = default_kalman_td(2)

	p1_stimuli, p1_rewards = prepare_stimuli_rewards("ZX", 10, "X > -")
	p2_stimuli, p2_rewards = prepare_stimuli_rewards("ZX", 10, "Z > X > +")
	test_stimuli, _ = prepare_stimuli_rewards("ZX", 1, "Z > -")

	return_value = process_stimuli_rewards(
		experimental_ktd,
		p1_stimuli,
		p1_rewards,
		(lambda ktd, exp: (ktd.weights_cov[1,1], ktd.old_kalman_gain[1]))
	)

	p1_posterior_variance, p1_kalman_gain = \
		list(map(lambda x : x[0], return_value)), \
		list(map(lambda x : x[1], return_value)),

	plt.plot(np.arange(1,len(p1_rewards)//2+1), p1_posterior_variance[::2])
	plt.xlabel("Pre-exposure Trial")
	plt.ylabel("Posterior Variance (X)")
	plt.title("Serial Compound Latent Inhibition")
	plt.show()

	plt.plot(np.arange(1,len(p1_rewards)//2+1), p1_kalman_gain[::2])
	plt.xlabel("Pre-exposure Trial")
	plt.ylabel("Kalman Gain (X)")
	plt.title("Serial Compound Latent Inhibition")
	plt.show()

	process_stimuli_rewards(experimental_ktd, p2_stimuli, p2_rewards)
	process_stimuli_rewards(control_ktd, p2_stimuli, p2_rewards)

	exp_test_reward = predict_reward(
		experimental_ktd,
		test_stimuli[1],
		test_stimuli[0]
	)
	ctrl_test_reward = predict_reward(
		control_ktd,
		test_stimuli[1],
		test_stimuli[0]
	)

	barplot = plt.bar([1,2], [exp_test_reward, ctrl_test_reward])
	plt.xticks([1,2], ["Pre", "No-Pre"])
	plt.ylabel("Value")
	plt.title("Serial Compound Latent Inhibition")
	plt.legend()
	plt.show()

	# return experimental_ktd, control_ktd
	return exp_test_reward, ctrl_test_reward

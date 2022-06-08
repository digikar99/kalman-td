
from KalmanTD import *
import numpy as np

def prepare_stimuli_rewards(all_stimuli_types, num_trials, paradigm):
	"""
	all_stimuli_types is used to map the stimuli to their one-hot-encoding;
	  order in which stimuli is listed in all_stimuli_types matters

	Returns (stimuli, rewards) tuple

	paradigm:
	  Special symbols:
		> for right-arrow, for separating
		/ for forward-slash (same as paper)
		; for semicolon (same as paper)
		+ for reward present (same as paper)
		- for reward absent (same as paper)
	"""

	num_stimuli_types = len(all_stimuli_types)
	stimuli_types = set(list(s for s in paradigm if s not in ">/;+- "))
	# print(stimuli_types)
	num_actual_stimuli_types = len(stimuli_types)
	assert num_stimuli_types >= num_actual_stimuli_types,\
		"Inferred number of stimuli types exceeds provided number of stimuli types"

	stimuli_index = dict()
	i = 0
	for stimuli_type in all_stimuli_types:
		stimuli_index[stimuli_type] = i
		i += 1

	trial_types = paradigm.split("; ")

	all_stimuli = []
	all_rewards = []
	for trial_type in trial_types:
		single_trial_stimuli = []
		single_trial_rewards = []
		subtrial_types = trial_type.split("/ ")
		# print(subtrial_types)
		for subtrial_type in subtrial_types:
			subtrial_components = subtrial_type.split("> ")
			presentation, expectation = "", subtrial_components[-1]
			expectation = expectation.replace(" ", "")
			if expectation not in ["+", "-"]: expectation = "-"
			else: subtrial_components = subtrial_components[:-1]

			for subtrial_component in subtrial_components:
				one_hot_encoding = np.zeros((num_stimuli_types, 1))
				subtrial_component = subtrial_component.replace(" ", "")
				for letter in subtrial_component:
					one_hot_encoding[stimuli_index[letter]] = 1
				single_trial_stimuli.append(one_hot_encoding)
				single_trial_rewards.append([0])

			assert len(expectation) == 1, "Don't know how to handle expectation " + expectation
			single_trial_stimuli.append(np.zeros((num_stimuli_types, 1)))
			if expectation == "+":
				single_trial_rewards.append([1])
			elif expectation == "-":
				single_trial_rewards.append([0])
			else:
				raise Exception("Don't know how to handle subtrial of type: " + subtrial_type)

		stimuli = np.tile(single_trial_stimuli, [num_trials, 1, 1])
		rewards = np.tile(single_trial_rewards, [num_trials, 1])

		all_stimuli.append(stimuli)
		all_rewards.append(rewards)

	stimuli = np.concatenate(all_stimuli)
	rewards = np.concatenate(all_rewards)
	return stimuli, rewards

def process_stimuli_rewards(ktd:KalmanTD, stimuli, rewards, callback=None):
	"""
	callback : function, called after each trial, and its results are accumulated
	  into a list which forms the return value of this function.
	  The function should take two arguments:
	  - first is the ktd supplied,
	  - and, second is the expected_reward
	"""
	accumulated_values = []
	for i in range(1,len(rewards)):
		expected_reward = predict_reward(ktd, stimuli[i], stimuli[i-1])
		update(ktd, expected_reward, rewards[i], stimuli[i], stimuli[i-1])
		if callback is not None:
			accumulated_values.append(callback(ktd, expected_reward))
	return accumulated_values

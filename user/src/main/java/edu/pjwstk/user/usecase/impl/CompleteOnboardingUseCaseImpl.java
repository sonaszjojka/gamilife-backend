package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.core.exception.common.domain.ResetPasswordGenericException;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.dto.service.UserDetailsDto;
import edu.pjwstk.user.persistence.UserMapper;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.CompleteOnboardingUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@AllArgsConstructor
public class CompleteOnboardingUseCaseImpl implements CompleteOnboardingUseCase {

    private final UserRepository userRepository;

    @Override
    @Transactional
    public UserDetailsDto execute(UUID userId) {
        User user = userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User with id:" + userId + " not found!"));

        user.setTutorialCompleted(true);
        userRepository.save(user);

        return new UserDetailsDto(
                user.getId(),
                user.getFirstName(),
                user.getLastName(),
                user.getEmail(),
                user.getUsername(),
                user.getDateOfBirth(),
                user.getExperience(),
                user.getMoney(),
                user.isSendBudgetReports(),
                user.isProfilePublic(),
                user.isEmailVerified(),
                user.isTutorialCompleted()
        );
    }
}

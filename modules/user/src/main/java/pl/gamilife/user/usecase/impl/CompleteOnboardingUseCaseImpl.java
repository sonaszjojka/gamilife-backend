package pl.gamilife.user.usecase.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.infrastructure.core.exception.common.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.dto.service.UserDetailsDto;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.CompleteOnboardingUseCase;

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

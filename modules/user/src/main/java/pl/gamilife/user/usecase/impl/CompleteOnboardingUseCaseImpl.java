package pl.gamilife.user.usecase.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.gamification.GamificationApi;
import pl.gamilife.api.gamification.dto.GamificationValuesDto;
import pl.gamilife.infrastructure.core.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.dto.service.UserDetailsDto;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.CompleteOnboardingUseCase;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class CompleteOnboardingUseCaseImpl implements CompleteOnboardingUseCase {

    private final UserRepository userRepository;
    private final GamificationApi gamificationApi;

    @Override
    public UserDetailsDto execute(UUID userId) {
        User user = userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User with id:" + userId + " not found!"));

        GamificationValuesDto startingGamificationValues = gamificationApi.getStartingGamificationValues();
        GamificationValuesDto postOnboardingGamificationValues = gamificationApi.getGamificationValuesForCompletedOnboarding();

        user.setTutorialCompleted(true);
        if (startingGamificationValues.level() == user.getLevel()) {
            user.grantExperience(postOnboardingGamificationValues.experience());
            user.grantMoney(postOnboardingGamificationValues.money());
            user.setLevel(postOnboardingGamificationValues.level());
        }
        user = userRepository.save(user);

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

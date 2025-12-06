package pl.gamilife.user.usecase.impl;

import edu.pjwstk.api.gamification.GamificationApi;
import edu.pjwstk.api.gamification.dto.GetRequiredExperienceByLevelIdResult;
import edu.pjwstk.core.exception.common.domain.ResetPasswordGenericException;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.dto.service.UserDetailsDto;
import edu.pjwstk.user.persistence.UserMapper;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.CompleteOnboardingUseCase;
import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.event.OnboardingCompletedEvent;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
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
    private ApplicationEventPublisher eventPublisher;
    private final GamificationApi gamificationApi;

    @Override
    public UserDetailsDto execute(UUID userId) {
        User user = userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User with id:" + userId + " not found!"));

        user.setTutorialCompleted(true);
        user = userRepository.save(user);

        eventPublisher.publishEvent(new OnboardingCompletedEvent(user.getId()));

        GetRequiredExperienceByLevelIdResult result  = gamificationApi.getRequiredExperienceByLevelId(user.getLevel()+1);

        return new UserDetailsDto(
                user.getId(),
                user.getFirstName(),
                user.getLastName(),
                user.getEmail(),
                user.getUsername(),
                user.getDateOfBirth(),
                user.getExperience(),
                user.getLevel(),
                result.requiredExperience(),
                user.getMoney(),
                user.isSendBudgetReports(),
                user.isProfilePublic(),
                user.isEmailVerified(),
                user.isTutorialCompleted()
        );
    }
}

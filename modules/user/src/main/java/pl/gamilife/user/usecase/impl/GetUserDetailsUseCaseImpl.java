package pl.gamilife.user.usecase.impl;

import edu.pjwstk.api.gamification.GamificationApi;
import edu.pjwstk.api.gamification.dto.GetRequiredExperienceByLevelIdResult;
import edu.pjwstk.api.gamification.dto.StartingGamificationValuesDto;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.dto.service.UserDetailsDto;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.GetUserDetailsUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.dto.service.UserDetailsDto;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.GetUserDetailsUseCase;

import java.util.UUID;

@Service
@AllArgsConstructor
public class GetUserDetailsUseCaseImpl implements GetUserDetailsUseCase {

    private final UserRepository userRepository;
    private final GamificationApi gamificationApi;

    @Override
    @Transactional(readOnly = true)
    public UserDetailsDto execute(UUID userId) {
        User user = userRepository
                .getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

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

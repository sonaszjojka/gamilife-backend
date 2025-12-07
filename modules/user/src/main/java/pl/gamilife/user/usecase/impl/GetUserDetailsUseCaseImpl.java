package pl.gamilife.user.usecase.impl;

import edu.pjwstk.api.gamification.GamificationApi;
import edu.pjwstk.api.gamification.dto.GetRequiredExperienceByLevelIdResult;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.dto.response.UserDetailsResponse;
import edu.pjwstk.user.dto.response.UserFullDetailsResponse;
import edu.pjwstk.user.dto.response.UserPrivateDetailsResponse;
import edu.pjwstk.user.dto.response.UserPublicDetailsResponse;
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
    public UserDetailsResponse execute(String requesterEmail, UUID targetUserId) {

        User user = userRepository.getUserById(targetUserId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        boolean isOwner = requesterEmail.equals(user.getEmail());
        if (!isOwner && user.isProfilePublic()) {
            return UserPublicDetailsResponse.from(user);
        }else if(!isOwner){
            return UserPrivateDetailsResponse.from(user);
        }

        var result = gamificationApi.getRequiredExperienceByLevelId(user.getLevel() + 1);
        return new UserFullDetailsResponse(
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
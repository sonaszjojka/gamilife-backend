package pl.gamilife.app.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.gamification.GamificationApi;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.UserDetailsDto;
import pl.gamilife.app.dto.UserDetailsResponse;
import pl.gamilife.app.dto.UserFullDetailsResponse;
import pl.gamilife.app.dto.UserPrivateDetailsResponse;
import pl.gamilife.app.dto.UserPublicDetailsResponse;
import pl.gamilife.app.service.UserDetailsService;

import java.util.UUID;

@Service
@AllArgsConstructor
public class UserDetailsServiceImpl implements UserDetailsService {

    private final UserApi userApi;
    private final GamificationApi gamificationApi;

    @Override
    public UserDetailsResponse getUserDetails(UUID currentUserId, UUID targetUserId) {
        UserDetailsDto userDetails = userApi.getUserDetails(targetUserId);

        boolean isOwner = currentUserId.equals(targetUserId);
        if (!isOwner && userDetails.isProfilePublic()) {
            return buildPublicDetailResponse(userDetails);
        } else if (!isOwner) {
            return buildPrivateDetailsResponse(userDetails);
        }

        Integer requiredForNextLevel = gamificationApi.getExperienceRequiredForNextLevel(userDetails.level());
        return buildFullDetailsResponse(userDetails, requiredForNextLevel);
    }

    private UserPrivateDetailsResponse buildPrivateDetailsResponse(UserDetailsDto dto) {
        return new UserPrivateDetailsResponse(
                dto.id(),
                dto.username(),
                dto.isProfilePublic()
        );
    }

    private UserPublicDetailsResponse buildPublicDetailResponse(UserDetailsDto dto) {
        return new UserPublicDetailsResponse(
                dto.id(),
                dto.firstName(),
                dto.lastName(),
                dto.username(),
                dto.level(),
                dto.money(),
                dto.isTutorialCompleted()
        );
    }

    private UserFullDetailsResponse buildFullDetailsResponse(UserDetailsDto dto, Integer requiredForNextLevel) {
        return new UserFullDetailsResponse(
                dto.id(),
                dto.firstName(),
                dto.lastName(),
                dto.email(),
                dto.username(),
                dto.dateOfBirth(),
                dto.experience(),
                dto.level(),
                requiredForNextLevel,
                dto.money(),
                dto.sendBudgetReports(),
                dto.isProfilePublic(),
                dto.isEmailVerified(),
                dto.isTutorialCompleted()
        );
    }
}

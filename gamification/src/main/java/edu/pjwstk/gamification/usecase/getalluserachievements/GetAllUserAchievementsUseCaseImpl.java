package edu.pjwstk.gamification.usecase.getalluserachievements;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.gamification.repository.AchievementRepository;
import edu.pjwstk.gamification.repository.query.AchievementDetailsDto;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
public class GetAllUserAchievementsUseCaseImpl implements GetAllUserAchievementsUseCase {

    private final AchievementRepository achievementRepository;
    private final AuthApi authApi;

    @Override
    public GetAllUserAchievementsResult executeInternal(GetAllUserAchievementsCommand command) {
        CurrentUserDto currentUser = authApi.getCurrentUser();
        var userId = currentUser.userId();

        List<AchievementDetailsDto> achievementDetails = achievementRepository.findAllAchievementDetailsByUserId(userId);

        List<GetAllUserAchievementsResult.AchievementDto> achievementDtos = achievementDetails.stream()
                .map(details -> new GetAllUserAchievementsResult.AchievementDto(
                        details.id(),
                        details.name(),
                        details.description(),
                        details.imagePath(),
                        details.isUnlocked(),
                        details.progress().intValue(),
                        details.goal()
                ))
                .collect(Collectors.toList());

        return new GetAllUserAchievementsResult(achievementDtos);
    }
}

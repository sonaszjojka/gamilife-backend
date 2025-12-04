package pl.gamilife.gamification.usecase.getalluserachievements;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.gamification.repository.AchievementRepository;
import pl.gamilife.gamification.repository.query.AchievementDetailsDto;

import java.util.List;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
public class GetAllUserAchievementsUseCaseImpl implements GetAllUserAchievementsUseCase {

    private final AchievementRepository achievementRepository;
    private final AuthApi authApi;

    @Override
    public GetAllUserAchievementsResult execute(GetAllUserAchievementsCommand cmd) {
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
                        details.progress(),
                        details.goal()
                ))
                .collect(Collectors.toList());

        return new GetAllUserAchievementsResult(achievementDtos);
    }
}

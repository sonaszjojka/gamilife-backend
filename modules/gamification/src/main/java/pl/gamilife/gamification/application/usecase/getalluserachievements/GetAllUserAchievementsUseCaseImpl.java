package pl.gamilife.gamification.application.usecase.getalluserachievements;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.projection.UserAchievementDetails;
import pl.gamilife.gamification.domain.port.repository.AchievementRepository;

import java.util.List;
import java.util.stream.Collectors;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetAllUserAchievementsUseCaseImpl implements GetAllUserAchievementsUseCase {

    private final AchievementRepository achievementRepository;

    @Override
    public GetAllUserAchievementsResult execute(GetAllUserAchievementsCommand cmd) {
        List<UserAchievementDetails> userAchievementDetails = achievementRepository
                .findAllAchievementDetailsByUserId(cmd.userId());

        List<GetAllUserAchievementsResult.AchievementDto> achievements = userAchievementDetails.stream()
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

        return new GetAllUserAchievementsResult(achievements);
    }
}

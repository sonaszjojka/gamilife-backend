package pl.gamilife.gamification.infrastructure.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.gamification.GamificationApi;
import pl.gamilife.api.gamification.dto.GamificationUserDetails;
import pl.gamilife.gamification.application.usecase.getgamificationuser.GetGamificationUserCommand;
import pl.gamilife.gamification.application.usecase.getgamificationuser.GetGamificationUserResult;
import pl.gamilife.gamification.application.usecase.getgamificationuser.GetGamificationUserUseCase;

import java.util.UUID;

@Service
@AllArgsConstructor
public class GamificationApiImpl implements GamificationApi {

    private final GetGamificationUserUseCase getGamificationUserUseCase;

    @Override
    public GamificationUserDetails getGamificationUserDetails(UUID userId) {
        GetGamificationUserResult user = getGamificationUserUseCase.execute(new GetGamificationUserCommand(userId));

        return new GamificationUserDetails(
                user.userId(),
                user.requiredExperienceForNextLevel()
        );
    }
}

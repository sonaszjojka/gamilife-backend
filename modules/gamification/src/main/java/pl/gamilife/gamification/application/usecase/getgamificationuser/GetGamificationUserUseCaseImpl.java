package pl.gamilife.gamification.application.usecase.getgamificationuser;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.model.projection.GamificationUser;
import pl.gamilife.gamification.domain.port.context.UserContext;
import pl.gamilife.gamification.domain.service.LevelService;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;

import java.util.Optional;

@Service
@AllArgsConstructor
@Transactional(readOnly = true)
public class GetGamificationUserUseCaseImpl implements GetGamificationUserUseCase {

    private final UserContext userContext;
    private final LevelService levelService;

    @Override
    public GetGamificationUserResult execute(GetGamificationUserCommand cmd) {
        GamificationUser user = userContext.getUserById(cmd.userId()).orElseThrow(
                () -> new UserNotFoundException("User not found")
        );

        Optional<Level> nextLevel = levelService.getNextLevel(user.level());
        Integer requiredExpForNextLevel = nextLevel.map(Level::getRequiredExperience).orElse(null);

        return new GetGamificationUserResult(
                user.userId(),
                user.username(),
                user.level(),
                user.experience(),
                user.money(),
                requiredExpForNextLevel
        );
    }
}

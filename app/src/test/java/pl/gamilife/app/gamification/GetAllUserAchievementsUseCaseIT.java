package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getalluserachievements.GetAllUserAchievementsCommand;
import pl.gamilife.gamification.application.usecase.getalluserachievements.GetAllUserAchievementsResult;
import pl.gamilife.gamification.application.usecase.getalluserachievements.GetAllUserAchievementsUseCase;
import pl.gamilife.gamification.application.usecase.processtaskcompletion.ProcessTaskCompletionCommand;
import pl.gamilife.gamification.application.usecase.processtaskcompletion.ProcessTaskCompletionUseCase;
import pl.gamilife.user.persistence.User;

import static org.assertj.core.api.Assertions.assertThat;

class GetAllUserAchievementsUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private GetAllUserAchievementsUseCase getAllUserAchievementsUseCase;

    @Autowired
    private ProcessTaskCompletionUseCase processTaskCompletionUseCase;

    @Test
    @DisplayName("Should return user achievements")
    void shouldReturnUserAchievements() {
        // given
        User user = createUserWithStats();
        processTaskCompletionUseCase.execute(new ProcessTaskCompletionCommand(user.getId(), true));

        // when
        GetAllUserAchievementsResult result = getAllUserAchievementsUseCase.execute(
                new GetAllUserAchievementsCommand(user.getId())
        );

        flushAndClear();

        // then
        assertThat(result.achievements()).isNotNull();
    }
}
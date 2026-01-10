package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsResult;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsUseCase;
import pl.gamilife.gamification.application.usecase.processpomodorotaskcompletion.ProcessPomodoroTaskCompletionCommand;
import pl.gamilife.gamification.application.usecase.processpomodorotaskcompletion.ProcessPomodoroTaskCompletionUseCase;
import pl.gamilife.gamification.application.usecase.rollbackpomodorotaskcompletion.RollbackPomodoroTaskCompletionCommand;
import pl.gamilife.gamification.application.usecase.rollbackpomodorotaskcompletion.RollbackPomodoroTaskCompletionUseCase;
import pl.gamilife.user.persistence.User;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class RollbackPomodoroTaskCompletionUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private RollbackPomodoroTaskCompletionUseCase rollbackPomodoroTaskCompletionUseCase;

    @Autowired
    private ProcessPomodoroTaskCompletionUseCase processPomodoroTaskCompletionUseCase;

    @Autowired
    private GetUserStatisticsUseCase getUserStatisticsUseCase;

    @Test
    @DisplayName("Should rollback pomodoro task completion progress")
    void shouldRollbackPomodoroTaskCompletionProgress() {
        // given
        User user = createUserWithStats();
        processPomodoroTaskCompletionUseCase.execute(new ProcessPomodoroTaskCompletionCommand(user.getId(), false));

        // when
        rollbackPomodoroTaskCompletionUseCase.execute(new RollbackPomodoroTaskCompletionCommand(user.getId()));

        flushAndClear();

        // then
        List<GetUserStatisticsResult> statistics = getUserStatisticsUseCase.execute(new GetUserStatisticsCommand(user.getId()));
        assertThat(statistics).anyMatch(stat -> "Pomodoro Tasks Completed".equals(stat.statisticType().type()) && stat.count() == 0);
    }
}
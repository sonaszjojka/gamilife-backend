package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsResult;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsUseCase;
import pl.gamilife.gamification.application.usecase.processtaskcompletion.ProcessTaskCompletionCommand;
import pl.gamilife.gamification.application.usecase.processtaskcompletion.ProcessTaskCompletionUseCase;
import pl.gamilife.gamification.application.usecase.rollbacktaskcompletion.RollbackTaskCompletionCommand;
import pl.gamilife.gamification.application.usecase.rollbacktaskcompletion.RollbackTaskCompletionUseCase;
import pl.gamilife.user.persistence.User;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class RollbackTaskCompletionUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private RollbackTaskCompletionUseCase rollbackTaskCompletionUseCase;

    @Autowired
    private ProcessTaskCompletionUseCase processTaskCompletionUseCase;

    @Autowired
    private GetUserStatisticsUseCase getUserStatisticsUseCase;

    @Test
    @DisplayName("Should rollback task completion progress")
    void shouldRollbackTaskCompletionProgress() {
        // given
        User user = createUserWithStats();
        processTaskCompletionUseCase.execute(new ProcessTaskCompletionCommand(user.getId(), false));

        // when
        rollbackTaskCompletionUseCase.execute(new RollbackTaskCompletionCommand(user.getId()));

        flushAndClear();

        // then
        List<GetUserStatisticsResult> statistics = getUserStatisticsUseCase.execute(new GetUserStatisticsCommand(user.getId()));
        assertThat(statistics).anyMatch(stat -> "Completed Tasks".equals(stat.statisticType().type()) && stat.count() == 0);
    }
}
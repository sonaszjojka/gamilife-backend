package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsResult;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsUseCase;
import pl.gamilife.gamification.application.usecase.processgrouptaskcompletion.ProcessGroupTaskCompletionCommand;
import pl.gamilife.gamification.application.usecase.processgrouptaskcompletion.ProcessGroupTaskCompletionUseCase;
import pl.gamilife.gamification.application.usecase.rollbackgrouptaskcompletion.RollbackGroupTaskCompletionCommand;
import pl.gamilife.gamification.application.usecase.rollbackgrouptaskcompletion.RollbackGroupTaskCompletionUseCase;
import pl.gamilife.user.persistence.User;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class RollbackGroupTaskCompletionUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private RollbackGroupTaskCompletionUseCase rollbackGroupTaskCompletionUseCase;

    @Autowired
    private ProcessGroupTaskCompletionUseCase processGroupTaskCompletionUseCase;

    @Autowired
    private GetUserStatisticsUseCase getUserStatisticsUseCase;

    @Test
    @DisplayName("Should rollback group task completion progress")
    void shouldRollbackGroupTaskCompletionProgress() {
        // given
        User user = createUserWithStats();
        processGroupTaskCompletionUseCase.execute(new ProcessGroupTaskCompletionCommand(List.of(user.getId()), false));

        // when
        rollbackGroupTaskCompletionUseCase.execute(new RollbackGroupTaskCompletionCommand(List.of(user.getId())));

        flushAndClear();

        // then
        List<GetUserStatisticsResult> statistics = getUserStatisticsUseCase.execute(new GetUserStatisticsCommand(user.getId()));
        assertThat(statistics).anyMatch(stat -> "Group Tasks Completed".equals(stat.statisticType().type()) && stat.count() == 0);
    }
}
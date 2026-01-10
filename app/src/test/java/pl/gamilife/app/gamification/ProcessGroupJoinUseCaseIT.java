package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsResult;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsUseCase;
import pl.gamilife.gamification.application.usecase.processgroupjoin.ProcessGroupJoinCommand;
import pl.gamilife.gamification.application.usecase.processgroupjoin.ProcessGroupJoinUseCase;
import pl.gamilife.user.persistence.User;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class ProcessGroupJoinUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private ProcessGroupJoinUseCase processGroupJoinUseCase;

    @Autowired
    private GetUserStatisticsUseCase getUserStatisticsUseCase;

    @Test
    @DisplayName("Should register progress when joining group for the first time")
    void shouldRegisterProgressWhenJoiningGroupFirstTime() {
        // given
        User user = createUserWithStats();

        // when
        processGroupJoinUseCase.execute(new ProcessGroupJoinCommand(user.getId(), true));

        flushAndClear();

        // then
        List<GetUserStatisticsResult> statistics = getUserStatisticsUseCase.execute(new GetUserStatisticsCommand(user.getId()));
        assertThat(statistics).anyMatch(stat -> "Joined Groups".equals(stat.statisticType().type()) && stat.count() == 1);
    }

    @Test
    @DisplayName("Should not register progress when joining group not for the first time")
    void shouldNotRegisterProgressWhenJoiningGroupNotFirstTime() {
        // given
        User user = createUserWithStats();

        // when
        processGroupJoinUseCase.execute(new ProcessGroupJoinCommand(user.getId(), false));

        flushAndClear();

        // then
        List<GetUserStatisticsResult> statistics = getUserStatisticsUseCase.execute(new GetUserStatisticsCommand(user.getId()));
        assertThat(statistics).anyMatch(stat -> "Joined Groups".equals(stat.statisticType().type()) && stat.count() == 0);
    }
}

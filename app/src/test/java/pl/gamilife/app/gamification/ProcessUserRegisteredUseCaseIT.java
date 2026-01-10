package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsResult;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsUseCase;
import pl.gamilife.gamification.application.usecase.processuserregistered.ProcessUserRegisteredCommand;
import pl.gamilife.gamification.application.usecase.processuserregistered.ProcessUserRegisteredUseCase;
import pl.gamilife.user.persistence.User;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class ProcessUserRegisteredUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private ProcessUserRegisteredUseCase processUserRegisteredUseCase;

    @Autowired
    private GetUserStatisticsUseCase getUserStatisticsUseCase;

    @Test
    @DisplayName("Should initialize statistics for registered user")
    void shouldInitializeStatistics() {
        // given
        User user = createUser();

        // when
        processUserRegisteredUseCase.execute(
                new ProcessUserRegisteredCommand(user.getId())
        );

        flushAndClear();

        // then
        List<GetUserStatisticsResult> result = getUserStatisticsUseCase.execute(
                new GetUserStatisticsCommand(user.getId())
        );
        assertThat(result).isNotEmpty();
    }
}
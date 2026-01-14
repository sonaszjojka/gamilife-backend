package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsResult;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsUseCase;
import pl.gamilife.user.persistence.User;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class GetUserStatisticsUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private GetUserStatisticsUseCase getUserStatisticsUseCase;

    @Test
    @DisplayName("Should return user statistics")
    void shouldReturnUserStatistics() {
        // given
        User user = createUserWithStats();

        flushAndClear();

        // when
        List<GetUserStatisticsResult> result = getUserStatisticsUseCase.execute(
                new GetUserStatisticsCommand(user.getId())
        );

        // then
        assertThat(result).isNotEmpty();
    }
}
package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsResult;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsUseCase;
import pl.gamilife.gamification.application.usecase.processitemacquisition.ProcessItemAcquisitionCommand;
import pl.gamilife.gamification.application.usecase.processitemacquisition.ProcessItemAcquisitionUseCase;
import pl.gamilife.user.persistence.User;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class ProcessItemAcquisitionUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private ProcessItemAcquisitionUseCase processItemAcquisitionUseCase;

    @Autowired
    private GetUserStatisticsUseCase getUserStatisticsUseCase;

    @Test
    @DisplayName("Should register item acquisition progress")
    void shouldRegisterItemAcquisitionProgress() {
        // given
        User user = createUserWithStats();

        // when
        processItemAcquisitionUseCase.execute(new ProcessItemAcquisitionCommand(user.getId(), 1));

        flushAndClear();

        // then
        List<GetUserStatisticsResult> statistics = getUserStatisticsUseCase.execute(new GetUserStatisticsCommand(user.getId()));
        assertThat(statistics).anyMatch(stat -> "Owned Items".equals(stat.statisticType().type()) && stat.count() == 1);
    }
}

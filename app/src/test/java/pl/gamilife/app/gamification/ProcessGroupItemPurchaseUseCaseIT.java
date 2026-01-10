package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsResult;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsUseCase;
import pl.gamilife.gamification.application.usecase.processgroupitempurchase.ProcessGroupItemPurchaseCommand;
import pl.gamilife.gamification.application.usecase.processgroupitempurchase.ProcessGroupItemPurchaseUseCase;
import pl.gamilife.user.persistence.User;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class ProcessGroupItemPurchaseUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private ProcessGroupItemPurchaseUseCase processGroupItemPurchaseUseCase;

    @Autowired
    private GetUserStatisticsUseCase getUserStatisticsUseCase;

    @Test
    @DisplayName("Should register progress for group item purchase")
    void shouldRegisterProgressForGroupItemPurchase() {
        // given
        User user = createUserWithStats();

        // when
        processGroupItemPurchaseUseCase.execute(new ProcessGroupItemPurchaseCommand(user.getId()));

        flushAndClear();

        // then
        List<GetUserStatisticsResult> statistics = getUserStatisticsUseCase.execute(new GetUserStatisticsCommand(user.getId()));
        assertThat(statistics).anyMatch(stat -> "Group Items Purchased".equals(stat.statisticType().type()) && stat.count() == 1);
    }
}

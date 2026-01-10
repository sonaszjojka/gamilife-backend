package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsResult;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsUseCase;
import pl.gamilife.gamification.application.usecase.processitempurchase.ProcessItemPurchaseCommand;
import pl.gamilife.gamification.application.usecase.processitempurchase.ProcessItemPurchaseUseCase;
import pl.gamilife.user.persistence.User;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class ProcessItemPurchaseUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private ProcessItemPurchaseUseCase processItemPurchaseUseCase;

    @Autowired
    private GetUserStatisticsUseCase getUserStatisticsUseCase;

    @Test
    @DisplayName("Should register item purchase progress")
    void shouldRegisterItemPurchaseProgress() {
        // given
        User user = createUserWithStats();

        // when
        processItemPurchaseUseCase.execute(new ProcessItemPurchaseCommand(user.getId(), 1));

        flushAndClear();

        // then
        List<GetUserStatisticsResult> statistics = getUserStatisticsUseCase.execute(new GetUserStatisticsCommand(user.getId()));
        assertThat(statistics).anyMatch(stat -> "Items Purchased".equals(stat.statisticType().type()) && stat.count() == 1);
    }
}
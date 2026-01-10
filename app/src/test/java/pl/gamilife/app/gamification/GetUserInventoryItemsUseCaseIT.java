package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsCommand;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsResult;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsUseCase;
import pl.gamilife.user.persistence.User;

import static org.assertj.core.api.Assertions.assertThat;

class GetUserInventoryItemsUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private GetUserInventoryItemsUseCase getUserInventoryItemsUseCase;

    @Test
    @DisplayName("Should return empty list for new user")
    void shouldReturnEmptyListForNewUser() {
        // given
        User user = createUserWithStats();

        // when
        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );

        flushAndClear();

        // then
        assertThat(result.content()).isEmpty();
    }
}
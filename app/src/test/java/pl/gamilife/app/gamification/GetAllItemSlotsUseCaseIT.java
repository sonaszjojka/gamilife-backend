package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getallitemslots.GetAllItemSlotsCommand;
import pl.gamilife.gamification.application.usecase.getallitemslots.GetAllItemSlotsResult;
import pl.gamilife.gamification.application.usecase.getallitemslots.GetAllItemSlotsUseCase;

import static org.assertj.core.api.Assertions.assertThat;

class GetAllItemSlotsUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private GetAllItemSlotsUseCase getAllItemSlotsUseCase;

    @Test
    @DisplayName("Should return all item slots")
    void shouldReturnAllItemSlots() {
        // when
        GetAllItemSlotsResult result = getAllItemSlotsUseCase.execute(new GetAllItemSlotsCommand());

        flushAndClear();

        // then
        assertThat(result.itemSlots()).isNotEmpty();
        assertThat(result.itemSlots()).extracting("name")
                .contains("Head", "Body", "Legs", "Feet", "Accessory", "Title", "Badge", "Background");
    }
}
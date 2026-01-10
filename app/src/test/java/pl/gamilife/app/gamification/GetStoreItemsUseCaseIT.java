package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.GetStoreItemsCommand;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.GetStoreItemsUseCase;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.StoreItemDto;
import pl.gamilife.shared.kernel.architecture.Page;

import static org.assertj.core.api.Assertions.assertThat;

class GetStoreItemsUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private GetStoreItemsUseCase getStoreItemsUseCase;

    @Test
    @DisplayName("Should return store items")
    void shouldReturnStoreItems() {
        // when
        Page<StoreItemDto> result = getStoreItemsUseCase.execute(
                new GetStoreItemsCommand(null, null, null, 0, 10)
        );

        flushAndClear();

        // then
        assertThat(result.content()).isNotEmpty();
    }

    @Test
    @DisplayName("Should filter store items by name")
    void shouldFilterStoreItemsByName() {
        // when
        Page<StoreItemDto> result = getStoreItemsUseCase.execute(
                new GetStoreItemsCommand("Neon Runner Cap", null, null, 0, 10)
        );

        flushAndClear();

        // then
        assertThat(result.content()).isNotEmpty();
        assertThat(result.content().iterator().next().name()).isEqualTo("Neon Runner Cap");
    }
}
package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.GetStoreItemsCommand;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.GetStoreItemsUseCase;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.StoreItemDto;
import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.List;

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
        assertThat(result.content())
                .isNotEmpty()
                .allMatch(i -> i.price() != null);
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
        assertThat(result.content())
                .isNotEmpty()
                .allMatch(i -> i.price() != null);
        assertThat(result.content().getFirst().name()).isEqualTo("Neon Runner Cap");
    }

    @Test
    @DisplayName("Should filter store items by item slot")
    void shouldFilterStoreItemsByItemSlot() {
        // when
        Page<StoreItemDto> result = getStoreItemsUseCase.execute(new GetStoreItemsCommand(
                null,
                List.of(ItemSlotEnum.HEAD.getItemSlotId()),
                null,
                0,
                10
        ));

        flushAndClear();

        // then
        assertThat(result.content())
                .isNotEmpty()
                .allMatch(i -> i.price() != null
                        && i.itemSlot().id().equals(ItemSlotEnum.HEAD.getItemSlotId())
                );
    }

    @Test
    @DisplayName("Should filter store items by rarity")
    void shouldFilterStoreItemsByRarity() {
        // when
        Page<StoreItemDto> result = getStoreItemsUseCase.execute(new GetStoreItemsCommand(
                null,
                null,
                List.of(RarityEnum.COMMON.getRarityId()),
                0,
                10
        ));

        flushAndClear();

        // then
        assertThat(result.content())
                .isNotEmpty()
                .allMatch(i -> i.price() != null
                        && i.rarity().id().equals(RarityEnum.COMMON.getRarityId())
                );
    }

    @Test
    @DisplayName("Should return store items with pagination")
    void shouldReturnStoreItemsWithPagination() {
        // when
        Page<StoreItemDto> result = getStoreItemsUseCase.execute(
                new GetStoreItemsCommand(null, null, null, 0, 1)
        );

        flushAndClear();

        // then
        assertThat(result.content())
                .hasSize(1)
                .allMatch(i -> i.price() != null);
        assertThat(result.totalPages()).isGreaterThan(1);
    }
}

package pl.gamilife.gamification.domain.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;

class ItemTest {

    @Test
    void shouldReturnCorrectItemSlotEnum_whenValidIdIsProvided() {
        // given
        ItemSlotEnum expectedEnum = ItemSlotEnum.HEAD;
        Item item = Instancio.of(Item.class)
                .set(field(Item::getItemSlotId), expectedEnum.getItemSlotId())
                .create();

        // when
        ItemSlotEnum result = item.getItemSlotEnum();

        // then
        assertThat(result).isEqualTo(expectedEnum);
    }

    @Test
    void shouldThrowException_whenItemSlotIdIsInvalid() {
        // given
        int invalidId = 999;
        Item item = Instancio.of(Item.class)
                .set(field(Item::getItemSlotId), invalidId)
                .create();

        // when
        Throwable thrown = catchThrowable(item::getItemSlotEnum);

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Invalid itemSlotId: " + invalidId);
    }

    @Test
    void shouldReturnCorrectRarityEnum_whenValidIdIsProvided() {
        // given
        RarityEnum expectedEnum = RarityEnum.COMMON;
        Item item = Instancio.of(Item.class)
                .set(field(Item::getRarityId), expectedEnum.getRarityId())
                .create();

        // when
        RarityEnum result = item.getRarityEnum();

        // then
        assertThat(result).isEqualTo(expectedEnum);
    }

    @Test
    void shouldThrowException_whenRarityIdIsInvalid() {
        // given
        int invalidId = 999;
        Item item = Instancio.of(Item.class)
                .set(field(Item::getRarityId), invalidId)
                .create();

        // when
        Throwable thrown = catchThrowable(item::getRarityEnum);

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Invalid rarityId: " + invalidId);
    }

    @Test
    void shouldReturnTrue_whenPriceIsNotNull() {
        // given
        Item item = Instancio.of(Item.class)
                .set(field(Item::getPrice), 100)
                .create();

        // when
        boolean result = item.isForSale();

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldReturnFalse_whenPriceIsNull() {
        // given
        Item item = Instancio.of(Item.class)
                .set(field(Item::getPrice), null)
                .create();

        // when
        boolean result = item.isForSale();

        // then
        assertThat(result).isFalse();
    }
}

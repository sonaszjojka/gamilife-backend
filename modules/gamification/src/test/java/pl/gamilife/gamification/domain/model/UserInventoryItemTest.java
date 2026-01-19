package pl.gamilife.gamification.domain.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.gamification.domain.exception.UserDoesNotHaveEnoughItemsException;

import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;

class UserInventoryItemTest {

    @Test
    void shouldCreateUserInventoryItem_whenValidDataIsProvided() {
        // given
        UUID userId = UUID.randomUUID();
        Item item = Instancio.create(Item.class);
        Integer quantity = 5;

        // when
        UserInventoryItem result = UserInventoryItem.create(userId, item, quantity);

        // then
        assertThat(result).isNotNull();
        assertThat(result.getUserId()).isEqualTo(userId);
        assertThat(result.getItem()).isEqualTo(item);
        assertThat(result.getQuantity()).isEqualTo(quantity);
    }

    @Test
    void shouldThrowExceptionDuringCreate_whenUserIdIsNull() {
        // given
        UUID userId = null;
        Item item = Instancio.create(Item.class);
        Integer quantity = 5;

        // when
        Throwable thrown = catchThrowable(() -> UserInventoryItem.create(userId, item, quantity));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("User ID cannot be null");
    }

    @Test
    void shouldThrowExceptionDuringCreate_whenItemIsNull() {
        // given
        UUID userId = UUID.randomUUID();
        Item item = null;
        Integer quantity = 5;

        // when
        Throwable thrown = catchThrowable(() -> UserInventoryItem.create(userId, item, quantity));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Item cannot be null");
    }

    @Test
    void shouldThrowExceptionDuringCreate_whenQuantityIsNull() {
        // given
        UUID userId = UUID.randomUUID();
        Item item = Instancio.create(Item.class);
        Integer quantity = null;

        // when
        Throwable thrown = catchThrowable(() -> UserInventoryItem.create(userId, item, quantity));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Quantity cannot be null");
    }

    @Test
    void shouldReturnTrue_whenItemBelongsToUser() {
        // given
        UUID userId = UUID.randomUUID();
        UserInventoryItem userInventoryItem = UserInventoryItem.create(userId, Instancio.create(Item.class), 1);

        // when
        boolean result = userInventoryItem.doesBelongTo(userId);

        // then
        assertThat(result).isTrue();
    }

    @Test
    void shouldReturnFalse_whenItemDoesNotBelongToUser() {
        // given
        UUID userId = UUID.randomUUID();
        UUID otherUserId = UUID.randomUUID();
        UserInventoryItem userInventoryItem = UserInventoryItem.create(userId, Instancio.create(Item.class), 1);

        // when
        boolean result = userInventoryItem.doesBelongTo(otherUserId);

        // then
        assertThat(result).isFalse();
    }

    @Test
    void shouldReturnFalseDuringBelongCheck_whenUserIdIsNull() {
        // given
        UUID userId = UUID.randomUUID();
        UserInventoryItem userInventoryItem = UserInventoryItem.create(userId, Instancio.create(Item.class), 1);

        // when
        boolean result = userInventoryItem.doesBelongTo(null);

        // then
        assertThat(result).isFalse();
    }

    @Test
    void shouldIncrementQuantity_whenPositiveAmountIsProvided() {
        // given
        UserInventoryItem userInventoryItem = UserInventoryItem.create(UUID.randomUUID(), Instancio.create(Item.class), 10);
        int increment = 5;

        // when
        userInventoryItem.incrementQuantityBy(increment);

        // then
        assertThat(userInventoryItem.getQuantity()).isEqualTo(15);
    }

    @Test
    void shouldThrowExceptionDuringIncrement_whenAmountIsZero() {
        // given
        UserInventoryItem userInventoryItem = UserInventoryItem.create(UUID.randomUUID(), Instancio.create(Item.class), 10);

        // when
        Throwable thrown = catchThrowable(() -> userInventoryItem.incrementQuantityBy(0));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Quantity increment be greater than 0");
    }

    @Test
    void shouldThrowExceptionDuringIncrement_whenAmountIsNegative() {
        // given
        UserInventoryItem userInventoryItem = UserInventoryItem.create(UUID.randomUUID(), Instancio.create(Item.class), 10);

        // when
        Throwable thrown = catchThrowable(() -> userInventoryItem.incrementQuantityBy(-1));

        // then
        assertThat(thrown)
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Quantity increment be greater than 0");
    }

    @Test
    void shouldQuickSellItems_whenUserHasEnoughQuantity() {
        // given
        Item item = Instancio.create(Item.class);
        UserInventoryItem userInventoryItem = UserInventoryItem.create(UUID.randomUUID(), item, 10);
        int sellAmount = 4;
        int expectedValue = item.getQuickSellValue() * sellAmount;

        // when
        int result = userInventoryItem.quickSellItems(sellAmount);

        // then
        assertThat(result).isEqualTo(expectedValue);
        assertThat(userInventoryItem.getQuantity()).isEqualTo(6);
    }

    @Test
    void shouldThrowExceptionDuringQuickSell_whenUserHasNotEnoughQuantity() {
        // given
        UserInventoryItem userInventoryItem = UserInventoryItem.create(UUID.randomUUID(), Instancio.create(Item.class), 5);

        // when
        Throwable thrown = catchThrowable(() -> userInventoryItem.quickSellItems(6));

        // then
        assertThat(thrown)
                .isInstanceOf(UserDoesNotHaveEnoughItemsException.class)
                .hasMessageContaining("User has only 5 such items in inventory");
    }

    @Test
    void shouldThrowExceptionDuringQuickSell_whenAmountIsNull() {
        // given
        UserInventoryItem userInventoryItem = UserInventoryItem.create(UUID.randomUUID(), Instancio.create(Item.class), 5);

        // when
        Throwable thrown = catchThrowable(() -> userInventoryItem.quickSellItems(null));

        // then
        assertThat(thrown).isInstanceOf(NullPointerException.class);
    }

    @Test
    void shouldChangeEquippedStatus_whenNewStatusIsDifferent() {
        // given
        UserInventoryItem userInventoryItem = UserInventoryItem.create(UUID.randomUUID(), Instancio.create(Item.class), 1);
        boolean newStatus = true;

        // when
        boolean result = userInventoryItem.changeEquippedStatus(newStatus);

        // then
        assertThat(result).isTrue();
        assertThat(userInventoryItem.getIsEquipped()).isTrue();
    }

    @Test
    void shouldNotChangeEquippedStatus_whenNewStatusIsSameAsCurrent() {
        // given
        UserInventoryItem userInventoryItem = UserInventoryItem.create(UUID.randomUUID(), Instancio.create(Item.class), 1);
        boolean sameStatus = false;

        // when
        boolean result = userInventoryItem.changeEquippedStatus(sameStatus);

        // then
        assertThat(result).isFalse();
        assertThat(userInventoryItem.getIsEquipped()).isFalse();
    }

    @Test
    void shouldReturnFalseWhenChangingToFalseAndItIsAlreadyFalse() {
        // given
        UserInventoryItem userInventoryItem = UserInventoryItem.create(UUID.randomUUID(), Instancio.create(Item.class), 1);

        // when
        boolean result = userInventoryItem.changeEquippedStatus(false);

        // then
        assertThat(result).isFalse();
    }

    @Test
    void shouldReturnFalseWhenChangingToTrueAndItIsAlreadyTrue() {
        // given
        UserInventoryItem userInventoryItem = UserInventoryItem.create(UUID.randomUUID(), Instancio.create(Item.class), 1);
        userInventoryItem.changeEquippedStatus(true);

        // when
        boolean result = userInventoryItem.changeEquippedStatus(true);

        // then
        assertThat(result).isFalse();
    }
}

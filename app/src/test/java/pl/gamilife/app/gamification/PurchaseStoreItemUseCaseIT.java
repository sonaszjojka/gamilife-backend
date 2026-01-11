package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsCommand;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsResult;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsUseCase;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemCommand;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemUseCase;
import pl.gamilife.gamification.domain.exception.ItemIsNotForSaleException;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaItemRepository;
import pl.gamilife.shared.kernel.exception.domain.UserHasNotEnoughMoneyException;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.jpa.JpaUserRepository;

import static org.assertj.core.api.Assertions.*;

class PurchaseStoreItemUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private JpaUserRepository userRepository;

    @Autowired
    private JpaItemRepository itemRepository;

    @Autowired
    private PurchaseStoreItemUseCase purchaseStoreItemUseCase;

    @Autowired
    private GetUserInventoryItemsUseCase getUserInventoryItemsUseCase;

    private Item getItem(boolean forSale) {
        return itemRepository.findAll()
                .stream()
                .filter(i -> i.isForSale() == forSale)
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("No items found in database. Check data.sql"));
    }

    @Test
    @DisplayName("Should purchase item successfully")
    void shouldPurchaseItem() {
        // given
        User user = createUserWithStats();
        Item item = getItem(true);
        int price = item.getPrice();
        user.grantMoney(price + 50);
        userRepository.save(user);

        // when
        purchaseStoreItemUseCase.execute(
                new PurchaseStoreItemCommand(user.getId(), item.getId())
        );

        flushAndClear();

        // then
        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        assertThat(result.content()).hasSize(1);
        assertThat(result.content().iterator().next().item().name()).isEqualTo(item.getName());
    }

    @Test
    @DisplayName("Should throw exception when item is not for sale")
    void shouldThrowExceptionWhenItemIsNotForSale() {
        // given
        User user = createUserWithStats();
        Item item = getItem(false);

        // when
        PurchaseStoreItemCommand cmd = new PurchaseStoreItemCommand(user.getId(), item.getId());
        Throwable throwable = catchThrowableOfType(
                ItemIsNotForSaleException.class,
                () -> purchaseStoreItemUseCase.execute(cmd)
        );

        flushAndClear();

        // then
        assertThat(throwable).isNotNull()
                .isInstanceOf(ItemIsNotForSaleException.class);
    }

    @Test
    @DisplayName("Should fail purchase if not enough money")
    void shouldFailPurchaseNotEnoughMoney() {
        // given
        User user = createUserWithStats();
        Item testitem = getItem(true);

        flushAndClear();

        // when / then
        PurchaseStoreItemCommand cmd = new PurchaseStoreItemCommand(user.getId(), testitem.getId());
        assertThatThrownBy(() -> purchaseStoreItemUseCase.execute(cmd))
                .isInstanceOf(UserHasNotEnoughMoneyException.class);
    }
}
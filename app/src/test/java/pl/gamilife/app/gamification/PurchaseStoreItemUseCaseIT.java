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
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaItemRepository;
import pl.gamilife.shared.kernel.exception.domain.UserHasNotEnoughMoneyException;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.jpa.JpaUserRepository;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class PurchaseStoreItemUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private JpaUserRepository userRepository;

    @Autowired
    private JpaItemRepository itemRepository;

    @Autowired
    private PurchaseStoreItemUseCase purchaseStoreItemUseCase;

    @Autowired
    private GetUserInventoryItemsUseCase getUserInventoryItemsUseCase;

    private Item getOrCreateItem() {
        return itemRepository.findAll().stream().findFirst()
                .orElseThrow(() -> new IllegalStateException("No items found in database. Check data.sql"));
    }

    @Test
    @DisplayName("Should purchase item successfully")
    void shouldPurchaseItem() {
        // given
        User user = createUserWithStats();
        Item item = getOrCreateItem();
        int price = item.getPrice() != null ? item.getPrice() : 100;
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
    @DisplayName("Should fail purchase if not enough money")
    void shouldFailPurchaseNotEnoughMoney() {
        // given
        User user = createUserWithStats();
        Item testitem = getOrCreateItem();

        flushAndClear();

        // when / then
        PurchaseStoreItemCommand cmd = new PurchaseStoreItemCommand(user.getId(), testitem.getId());
        assertThatThrownBy(() -> purchaseStoreItemUseCase.execute(cmd))
                .isInstanceOf(UserHasNotEnoughMoneyException.class);
    }
}
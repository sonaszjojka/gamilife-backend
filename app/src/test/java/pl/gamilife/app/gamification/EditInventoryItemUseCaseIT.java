package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.editinventoryitem.EditInventoryItemCommand;
import pl.gamilife.gamification.application.usecase.editinventoryitem.EditInventoryItemUseCase;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsCommand;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsResult;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsUseCase;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemCommand;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemUseCase;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaItemRepository;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.jpa.JpaUserRepository;

import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;

class EditInventoryItemUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private JpaUserRepository userRepository;

    @Autowired
    private JpaItemRepository itemRepository;

    @Autowired
    private PurchaseStoreItemUseCase purchaseStoreItemUseCase;

    @Autowired
    private GetUserInventoryItemsUseCase getUserInventoryItemsUseCase;

    @Autowired
    private EditInventoryItemUseCase editInventoryItemUseCase;

    private Item getItem() {
        return itemRepository.findAll().stream().findFirst()
                .orElseThrow(() -> new IllegalStateException("No items found in database. Check data.sql"));
    }

    @Test
    @DisplayName("Should update inventory item (equip/unequip)")
    void shouldUpdateInventoryItem() {
        // given
        User user = createUserWithStats();
        Item item = getItem();
        int price = item.getPrice() != null ? item.getPrice() : 100;
        user.grantMoney(price + 50);
        userRepository.save(user);

        purchaseStoreItemUseCase.execute(
                new PurchaseStoreItemCommand(user.getId(), item.getId())
        );

        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        UUID inventoryItemId = result.content().iterator().next().id();

        // when
        editInventoryItemUseCase.execute(
                new EditInventoryItemCommand(user.getId(), inventoryItemId, null, true)
        );

        flushAndClear();

        // then
        GetUserInventoryItemsResult updatedResult = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        assertThat(updatedResult.content().iterator().next().isEquipped()).isTrue();
    }
}
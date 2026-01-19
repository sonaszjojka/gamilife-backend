package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.editinventoryitem.EditInventoryItemCommand;
import pl.gamilife.gamification.application.usecase.editinventoryitem.EditInventoryItemResult;
import pl.gamilife.gamification.application.usecase.editinventoryitem.EditInventoryItemUseCase;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsCommand;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsResult;
import pl.gamilife.gamification.application.usecase.getuserinventoryitems.GetUserInventoryItemsUseCase;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemCommand;
import pl.gamilife.gamification.application.usecase.purchasestoreitem.PurchaseStoreItemUseCase;
import pl.gamilife.gamification.domain.exception.ForbiddenItemAccessException;
import pl.gamilife.gamification.domain.exception.InventoryItemNotFound;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaItemRepository;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.jpa.JpaUserRepository;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

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

    private Item getItemBySlotAndForSale(int slotId) {
        return itemRepository.findAll().stream()
                .filter(item -> item.getItemSlotId() == slotId && item.isForSale())
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("No items found for slot " + slotId));
    }

    private Item getAnotherItemForSameSlotAndForSale(Item firstItem) {
        return itemRepository.findAll().stream()
                .filter(item -> Objects.equals(item.getItemSlotId(), firstItem.getItemSlotId())
                        && !item.getId().equals(firstItem.getId())
                        && item.isForSale()
                )
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("No second item found for slot " + firstItem.getItemSlotId()));
    }

    @Test
    @DisplayName("Should equip inventory item")
    void shouldUpdateInventoryItem() {
        // given
        User user = createUserWithStats();
        Item item = getItem();
        user.grantMoney(1000);
        userRepository.save(user);

        purchaseStoreItemUseCase.execute(new PurchaseStoreItemCommand(user.getId(), item.getId()));

        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        UUID inventoryItemId = result.content().iterator().next().id();

        // when
        EditInventoryItemResult editResult = editInventoryItemUseCase.execute(
                new EditInventoryItemCommand(user.getId(), inventoryItemId, null, true)
        );

        flushAndClear();

        // then
        assertThat(editResult.newIsEquipped()).isTrue();

        GetUserInventoryItemsResult updatedResult = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        assertThat(updatedResult.content().iterator().next().isEquipped()).isTrue();
    }

    @Test
    @DisplayName("Should unequip inventory item")
    void shouldUnequipInventoryItem() {
        // given
        User user = createUserWithStats();
        Item item = getItem();
        user.grantMoney(1000);
        userRepository.save(user);

        purchaseStoreItemUseCase.execute(new PurchaseStoreItemCommand(user.getId(), item.getId()));

        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        UUID inventoryItemId = result.content().iterator().next().id();

        editInventoryItemUseCase.execute(new EditInventoryItemCommand(user.getId(), inventoryItemId, null, true));

        // when
        EditInventoryItemResult editResult = editInventoryItemUseCase.execute(
                new EditInventoryItemCommand(user.getId(), inventoryItemId, null, false)
        );

        flushAndClear();

        // then
        assertThat(editResult.newIsEquipped()).isFalse();

        GetUserInventoryItemsResult updatedResult = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        assertThat(updatedResult.content().iterator().next().isEquipped()).isFalse();
    }

    @Test
    @DisplayName("Should quick sell inventory items")
    void shouldQuickSellInventoryItems() {
        // given
        User user = createUserWithStats();
        Item item = getItem();
        user.grantMoney(1000);
        userRepository.save(user);

        for (int i = 0; i < 5; i++) {
            purchaseStoreItemUseCase.execute(new PurchaseStoreItemCommand(user.getId(), item.getId()));
        }

        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        UUID inventoryItemId = result.content().iterator().next().id();
        User userBeforeSale = userRepository.findById(user.getId()).orElseThrow();
        int moneyBeforeSale = userBeforeSale.getMoney();

        // when
        EditInventoryItemResult editResult = editInventoryItemUseCase.execute(
                new EditInventoryItemCommand(user.getId(), inventoryItemId, 2, null)
        );

        flushAndClear();

        // then
        int expectedMoney = moneyBeforeSale + (item.getQuickSellValue() * 2);

        assertThat(editResult.newQuantity()).isEqualTo(3);
        assertThat(editResult.newUserMoney()).isEqualTo(expectedMoney);

        User updatedUser = userRepository.findById(user.getId()).orElseThrow();
        assertThat(updatedUser.getMoney()).isEqualTo(expectedMoney);
    }

    @Test
    @DisplayName("Should delete inventory item when selling all quantity")
    void shouldDeleteInventoryItem_whenSellingAll() {
        // given
        User user = createUserWithStats();
        Item item = getItem();
        user.grantMoney(1000);
        userRepository.save(user);

        purchaseStoreItemUseCase.execute(new PurchaseStoreItemCommand(user.getId(), item.getId()));

        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        UUID inventoryItemId = result.content().iterator().next().id();

        // when
        EditInventoryItemResult editResult = editInventoryItemUseCase.execute(
                new EditInventoryItemCommand(user.getId(), inventoryItemId, 1, null)
        );

        flushAndClear();

        // then
        assertThat(editResult.userInventoryItemId()).isNull();

        GetUserInventoryItemsResult updatedResult = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        assertThat(updatedResult.content()).isEmpty();
    }

    @Test
    @DisplayName("Should auto-unequip item when equipping another in same slot")
    void shouldAutoUnequipItemOnSameSlot_whenOtherItemIsAlreadyEquippedThere() {
        // given
        User user = createUserWithStats();
        user.grantMoney(2000);
        userRepository.save(user);

        Item item1 = getItemBySlotAndForSale(1);
        Item item2 = getAnotherItemForSameSlotAndForSale(item1);

        purchaseStoreItemUseCase.execute(new PurchaseStoreItemCommand(user.getId(), item1.getId()));
        purchaseStoreItemUseCase.execute(new PurchaseStoreItemCommand(user.getId(), item2.getId()));

        GetUserInventoryItemsResult items = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );

        var content = (List<GetUserInventoryItemsResult.UserInventoryItemDto>) items.content();
        UUID invItem1Id = content.stream().filter(i -> i.itemId().equals(item1.getId())).findFirst().orElseThrow().id();
        UUID invItem2Id = content.stream().filter(i -> i.itemId().equals(item2.getId())).findFirst().orElseThrow().id();

        editInventoryItemUseCase.execute(new EditInventoryItemCommand(user.getId(), invItem1Id, null, true));

        // when
        editInventoryItemUseCase.execute(new EditInventoryItemCommand(user.getId(), invItem2Id, null, true));

        flushAndClear();

        // then
        GetUserInventoryItemsResult finalItems = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        var finalContent = (List<GetUserInventoryItemsResult.UserInventoryItemDto>) finalItems.content();

        boolean item1Equipped = finalContent.stream()
                .filter(i -> i.id().equals(invItem1Id))
                .findFirst()
                .orElseThrow()
                .isEquipped();
        boolean item2Equipped = finalContent.stream()
                .filter(i -> i.id().equals(invItem2Id))
                .findFirst()
                .orElseThrow()
                .isEquipped();

        assertThat(item1Equipped).isFalse();
        assertThat(item2Equipped).isTrue();
    }

    @Test
    @DisplayName("Should not change status when equipping already equipped item")
    void shouldNotChangeStatus_whenEquippingAlreadyEquippedItem() {
        // given
        User user = createUserWithStats();
        Item item = getItem();
        user.grantMoney(1000);
        userRepository.save(user);

        purchaseStoreItemUseCase.execute(new PurchaseStoreItemCommand(user.getId(), item.getId()));

        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        UUID inventoryItemId = result.content().iterator().next().id();

        editInventoryItemUseCase.execute(new EditInventoryItemCommand(user.getId(), inventoryItemId, null, true));

        // when
        EditInventoryItemResult editResult = editInventoryItemUseCase.execute(
                new EditInventoryItemCommand(user.getId(), inventoryItemId, null, true)
        );

        flushAndClear();

        // then
        assertThat(editResult.newIsEquipped()).isNull();

        GetUserInventoryItemsResult updatedResult = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );
        assertThat(updatedResult.content().iterator().next().isEquipped()).isTrue();
    }

    @Test
    @DisplayName("Should throw exception when accessing another user's item")
    void shouldThrowException_whenAccessingAnotherUserItem() {
        // given
        User user1 = createUserWithStats();
        User user2 = createUser();

        Item item = getItem();
        user1.grantMoney(1000);
        userRepository.save(user1);

        purchaseStoreItemUseCase.execute(new PurchaseStoreItemCommand(user1.getId(), item.getId()));

        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(new GetUserInventoryItemsCommand(
                user1.getId(),
                null,
                null,
                null,
                0,
                10
        ));
        UUID user1InventoryItemId = result.content().iterator().next().id();

        // when / then
        EditInventoryItemCommand cmd = new EditInventoryItemCommand(
                user2.getId(),
                user1InventoryItemId,
                null,
                true);
        assertThatThrownBy(() -> editInventoryItemUseCase.execute(cmd)).isInstanceOf(ForbiddenItemAccessException.class);
    }

    @Test
    @DisplayName("Should throw exception when item not found")
    void shouldThrowException_whenItemNotFound() {
        // given
        User user = createUserWithStats();
        UUID randomId = UUID.randomUUID();

        // when / then
        EditInventoryItemCommand cmd = new EditInventoryItemCommand(
                user.getId(),
                randomId,
                null,
                true
        );
        assertThatThrownBy(() -> editInventoryItemUseCase.execute(cmd)).isInstanceOf(InventoryItemNotFound.class);
    }
}

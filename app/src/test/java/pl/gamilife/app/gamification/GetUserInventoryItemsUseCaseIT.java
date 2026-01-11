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
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.jpa.JpaUserRepository;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class GetUserInventoryItemsUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private GetUserInventoryItemsUseCase getUserInventoryItemsUseCase;

    @Autowired
    private PurchaseStoreItemUseCase purchaseStoreItemUseCase;

    @Autowired
    private JpaItemRepository itemRepository;

    @Autowired
    private JpaUserRepository userRepository;

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

    @Test
    @DisplayName("Should return all items when no filter provided")
    void shouldReturnAllItemsWhenNoFilterProvided() {
        // given
        User user = createUserWithStats();
        grantMoney(user, 1000);
        Item item = getItemByName("Neon Runner Cap");
        purchaseItem(user, item);

        // when
        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 10)
        );

        flushAndClear();

        // then
        assertThat(result.content()).hasSize(1);
        assertThat(result.content().iterator().next().item().name()).isEqualTo("Neon Runner Cap");
    }

    @Test
    @DisplayName("Should return items filtered by name")
    void shouldReturnItemsFilteredByName() {
        // given
        User user = createUserWithStats();
        grantMoney(user, 1000);
        Item item1 = getItemByName("Neon Runner Cap");
        Item item2 = getItemByName("Synthweave Tee");
        purchaseItem(user, item1);
        purchaseItem(user, item2);

        // when
        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), "Neon", null, null, 0, 10)
        );

        flushAndClear();

        // then
        assertThat(result.content()).hasSize(1);
        assertThat(result.content().iterator().next().item().name()).isEqualTo("Neon Runner Cap");
    }

    @Test
    @DisplayName("Should return items filtered by slot")
    void shouldReturnItemsFilteredBySlot() {
        // given
        User user = createUserWithStats();
        grantMoney(user, 1000);
        Item headItem = getItemByName("Neon Runner Cap");
        Item bodyItem = getItemByName("Synthweave Tee");
        purchaseItem(user, headItem);
        purchaseItem(user, bodyItem);

        // when
        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, 1, null, 0, 10)
        );

        flushAndClear();

        // then
        assertThat(result.content()).hasSize(1);
        assertThat(result.content().iterator().next().item().itemSlot().name()).isEqualTo("Head");
    }

    @Test
    @DisplayName("Should return items filtered by rarity")
    void shouldReturnItemsFilteredByRarity() {
        // given
        User user = createUserWithStats();
        grantMoney(user, 1000);
        Item commonItem = getItemByName("Neon Runner Cap");
        purchaseItem(user, commonItem);

        // when
        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, 1, 0, 10)
        );

        flushAndClear();

        // then
        assertThat(result.content()).hasSize(1);
        assertThat(result.content().iterator().next().item().rarity().name()).isEqualTo("Common");
    }

    @Test
    @DisplayName("Should return items with pagination")
    void shouldReturnItemsWithPagination() {
        // given
        User user = createUserWithStats();
        grantMoney(user, 1000);
        Item item1 = getItemByName("Neon Runner Cap");
        Item item2 = getItemByName("Synthweave Tee");
        purchaseItem(user, item1);
        purchaseItem(user, item2);

        // when
        GetUserInventoryItemsResult result = getUserInventoryItemsUseCase.execute(
                new GetUserInventoryItemsCommand(user.getId(), null, null, null, 0, 1)
        );

        flushAndClear();

        // then
        assertThat(result.content()).hasSize(1);
        assertThat(result.totalPages()).isEqualTo(2);
        assertThat(result.totalElements()).isEqualTo(2);
    }

    private void grantMoney(User user, int amount) {
        user.grantMoney(amount);
        userRepository.save(user);
    }

    private Item getItemByName(String name) {
        List<Item> items = itemRepository.findAll();
        return items.stream()
                .filter(i -> i.getName().equals(name))
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("Item not found: " + name));
    }

    private void purchaseItem(User user, Item item) {
        purchaseStoreItemUseCase.execute(new PurchaseStoreItemCommand(user.getId(), item.getId()));
    }
}

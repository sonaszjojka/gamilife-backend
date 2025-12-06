package pl.gamilife.gamification.application.usecase.purchasestoreitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.exception.ItemIsNotForSaleException;
import pl.gamilife.gamification.domain.exception.ItemNotFoundException;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.UserInventoryItem;
import pl.gamilife.gamification.domain.model.projection.GamificationUser;
import pl.gamilife.gamification.domain.port.context.UserContext;
import pl.gamilife.gamification.domain.port.repository.ItemRepository;
import pl.gamilife.gamification.domain.service.UserInventoryService;
import pl.gamilife.infrastructure.core.exception.domain.UserHasNotEnoughMoneyException;
import pl.gamilife.infrastructure.core.exception.domain.UserNotFoundException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class PurchaseStoreItemUseCaseImpl implements PurchaseStoreItemUseCase {

    private final UserInventoryService userInventoryService;
    private final UserContext userContext;
    private final ItemRepository itemRepository;

    @Override
    public PurchaseStoreItemResult execute(PurchaseStoreItemCommand cmd) {
        GamificationUser user = getUser(cmd.userId());
        Item item = getItem(cmd.itemId());

        if (!item.isForSale()) {
            throw new ItemIsNotForSaleException("Item is not for sale");
        }

        if (user.money() < item.getPrice()) {
            throw new UserHasNotEnoughMoneyException("User has not enough money");
        }

        int newUserMoney = userContext.payForNewItem(user.userId(), item.getPrice());
        UserInventoryItem userInventoryItem = userInventoryService.addItemToUsersInventory(user.userId(), item);

        return new PurchaseStoreItemResult(
                userInventoryItem.getId(),
                item.getId(),
                userInventoryItem.getQuantity(),
                newUserMoney
        );
    }

    private GamificationUser getUser(UUID userId) {
        return userContext.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }


    private Item getItem(UUID itemId) {
        return itemRepository.findById(itemId)
                .orElseThrow(() -> new ItemNotFoundException("Item not found"));
    }
}

package pl.gamilife.gamification.usecase.purchasestoreitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.gamification.exception.domain.ItemIsNotForSaleException;
import pl.gamilife.gamification.exception.domain.ItemNotFoundException;
import pl.gamilife.gamification.model.Item;
import pl.gamilife.gamification.model.UserInventoryItem;
import pl.gamilife.gamification.repository.ItemRepository;
import pl.gamilife.gamification.service.UserInventoryService;
import pl.gamilife.infrastructure.core.exception.common.domain.UserHasNotEnoughMoneyException;
import pl.gamilife.infrastructure.core.exception.common.domain.UserNotFoundException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class PurchaseStoreItemUseCaseImpl implements PurchaseStoreItemUseCase {

    private final UserInventoryService userInventoryService;
    private final UserApi userApi;
    private final ItemRepository itemRepository;

    @Override
    public PurchaseStoreItemResult execute(PurchaseStoreItemCommand cmd) {
        BasicUserInfoApiDto user = getUser(cmd.userId());
        Item item = getItem(cmd.itemId());

        if (!item.isForSale()) {
            throw new ItemIsNotForSaleException("Item is not for sale");
        }

        if (user.money() < item.getPrice()) {
            throw new UserHasNotEnoughMoneyException("User has not enough money");
        }

        int newUserMoney = userApi.editUserMoneyBy(user.userId(), (-1) * item.getPrice());
        UserInventoryItem userInventoryItem = userInventoryService.addItemToUsersInventory(user.userId(), item);

        return new PurchaseStoreItemResult(
                userInventoryItem.getId(),
                item.getId(),
                userInventoryItem.getQuantity(),
                newUserMoney
        );
    }

    private BasicUserInfoApiDto getUser(UUID userId) {
        return userApi.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }


    private Item getItem(UUID itemId) {
        return itemRepository.findById(itemId)
                .orElseThrow(() -> new ItemNotFoundException("Item not found"));
    }
}

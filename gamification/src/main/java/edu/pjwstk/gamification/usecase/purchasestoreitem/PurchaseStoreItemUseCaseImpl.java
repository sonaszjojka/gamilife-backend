package edu.pjwstk.gamification.usecase.purchasestoreitem;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.core.exception.common.domain.UserHasNotEnoughMoneyException;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.gamification.exception.domain.ItemIsNotForSaleException;
import edu.pjwstk.gamification.exception.domain.ItemNotFoundException;
import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventoryItem;
import edu.pjwstk.gamification.repository.ItemRepository;
import edu.pjwstk.gamification.service.UserInventoryService;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class PurchaseStoreItemUseCaseImpl implements PurchaseStoreItemUseCase {

    private final UserInventoryService userInventoryService;
    private final UserApi userApi;
    private final ItemRepository itemRepository;

    @Override
    @Transactional
    public PurchaseStoreItemResult executeInternal(PurchaseStoreItemCommand cmd) {
        BasicUserInfoApiDto user = getUser(cmd.userId());
        Item item = getItem(cmd.itemId());

        if (!item.isForSale()) {
            throw new ItemIsNotForSaleException("Item is not for sale");
        }

        if (user.money() < item.getPrice()) {
            throw new UserHasNotEnoughMoneyException("User has not enough money");
        }

        int newUserMoney = userApi.editUserMoneyBy(user.userId(), (-1) * item.getPrice());
        UserInventoryItem userInventoryItem = userInventoryService.addItemToInventory(user, item);

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

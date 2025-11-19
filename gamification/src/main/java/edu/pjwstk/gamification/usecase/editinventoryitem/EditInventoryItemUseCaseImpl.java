package edu.pjwstk.gamification.usecase.editinventoryitem;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.gamification.exception.domain.ForbiddenItemAccessException;
import edu.pjwstk.gamification.exception.domain.InventoryItemNotFound;
import edu.pjwstk.gamification.exception.domain.UserDoesNotHaveEnoughItemsException;
import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventoryItem;
import edu.pjwstk.gamification.repository.UserInventoryItemRepository;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class EditInventoryItemUseCaseImpl implements EditInventoryItemUseCase {

    private final UserInventoryItemRepository userInventoryItemRepository;
    private final UserApi userApi;

    private static void checkIfUserCanSellGivenQuantityOfItems(UserInventoryItem userInventoryItem) {
        if (userInventoryItem.getQuantity() < 0) {
            throw new UserDoesNotHaveEnoughItemsException(
                    String.format("User has only %s such items in inventory", userInventoryItem.getQuantity())
            );
        }
    }

    @Override
    @Transactional
    public EditInventoryItemResult executeInternal(EditInventoryItemCommand cmd) {
        checkIfUserExists(cmd.userId());
        UserInventoryItem userInventoryItem = getUserInventoryItemWithItem(cmd.userInventoryItemId());

        if (!userInventoryItem.doesBelongTo(cmd.userId())) {
            throw new ForbiddenItemAccessException("User does not own this item");
        }

        var resultBuilder = EditInventoryItemResult.builder()
                .userInventoryItemId(userInventoryItem.getId());

        if (cmd.subtractQuantityBy() != null) {
            Item item = userInventoryItem.getItem();

            userInventoryItem.setQuantity(userInventoryItem.getQuantity() - cmd.subtractQuantityBy());

            checkIfUserCanSellGivenQuantityOfItems(userInventoryItem);
            int newUserMoney = userApi.editUserMoneyBy(cmd.userId(), item.getQuickSellValue() * cmd.subtractQuantityBy());

            if (userInventoryItem.getQuantity() == 0) {
                userInventoryItemRepository.delete(userInventoryItem);
            }

            resultBuilder.userInventoryItemId(null)
                    .newUserMoney(newUserMoney)
                    .newQuantity(userInventoryItem.getQuantity());

            if (userInventoryItem.getQuantity() == 0) {
                return resultBuilder.build();
            }
        }

        if (userInventoryItem.getIsEquipped() != cmd.isEquipped()) {
            userInventoryItem.setIsEquipped(cmd.isEquipped());
            resultBuilder.newIsEquipped(cmd.isEquipped());
        }

        userInventoryItemRepository.save(userInventoryItem);

        return resultBuilder.build();
    }

    private void checkIfUserExists(UUID userId) {
        userApi.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }

    private UserInventoryItem getUserInventoryItemWithItem(UUID userInventoryItemId) {
        return userInventoryItemRepository.findWithItemById(userInventoryItemId)
                .orElseThrow(() -> new InventoryItemNotFound("Item in inventory not found"));
    }
}
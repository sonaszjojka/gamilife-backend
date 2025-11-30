package edu.pjwstk.gamification.usecase.editinventoryitem;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.gamification.exception.domain.ForbiddenItemAccessException;
import edu.pjwstk.gamification.exception.domain.InventoryItemNotFound;
import edu.pjwstk.gamification.exception.domain.UserDoesNotHaveEnoughItemsException;
import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventoryItem;
import edu.pjwstk.gamification.repository.UserInventoryItemRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class EditInventoryItemUseCaseImpl implements EditInventoryItemUseCase {

    private final UserInventoryItemRepository userInventoryItemRepository;
    private final UserApi userApi;

    @Override
    public EditInventoryItemResult execute(EditInventoryItemCommand cmd) {
        checkIfUserExists(cmd.userId());
        UserInventoryItem userInventoryItem = getUserInventoryItemWithItem(cmd.userInventoryItemId());

        if (!userInventoryItem.doesBelongTo(cmd.userId())) {
            throw new ForbiddenItemAccessException("User does not own this item");
        }

        var resultBuilder = EditInventoryItemResult.builder()
                .userInventoryItemId(userInventoryItem.getId());

        if (cmd.subtractQuantityBy() != null) {
            Item item = userInventoryItem.getItem();

            int newItemQuantity = userInventoryItem.getQuantity() - cmd.subtractQuantityBy();
            if (newItemQuantity < 0) {
                throw new UserDoesNotHaveEnoughItemsException(
                        String.format("User has only %s such items in inventory", userInventoryItem.getQuantity())
                );
            }

            int newUserMoney = userApi.editUserMoneyBy(cmd.userId(), item.getQuickSellValue() * cmd.subtractQuantityBy());

            resultBuilder.newUserMoney(newUserMoney)
                    .newQuantity(newItemQuantity);

            if (newItemQuantity == 0) {
                userInventoryItemRepository.delete(userInventoryItem);

                return resultBuilder.userInventoryItemId(null)
                        .build();
            }

            userInventoryItem.setQuantity(userInventoryItem.getQuantity() - cmd.subtractQuantityBy());
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
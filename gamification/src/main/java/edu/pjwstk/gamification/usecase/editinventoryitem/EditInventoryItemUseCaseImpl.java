package edu.pjwstk.gamification.usecase.editinventoryitem;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.gamification.exception.domain.ForbiddenItemAccessException;
import edu.pjwstk.gamification.exception.domain.InventoryItemNotFound;
import edu.pjwstk.gamification.exception.domain.UserDoesNotHaveEnoughItemsException;
import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventory;
import edu.pjwstk.gamification.repository.UserInventoryRepository;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class EditInventoryItemUseCaseImpl implements EditInventoryItemUseCase {

    private final UserInventoryRepository userInventoryRepository;
    private final UserApi userApi;

    private static void checkIfUserCanSellGivenQuantityOfItems(UserInventory userInventory) {
        if (userInventory.getQuantity() < 0) {
            throw new UserDoesNotHaveEnoughItemsException(
                    String.format("User has only %s such items in inventory", userInventory.getQuantity())
            );
        }
    }

    @Override
    @Transactional
    public EditInventoryItemResult executeInternal(EditInventoryItemCommand cmd) {
        checkIfUserExists(cmd.userId());
        UserInventory userInventory = getUserInventoryWithItem(cmd.userInventoryId());

        if (!userInventory.doesBelongTo(cmd.userId())) {
            throw new ForbiddenItemAccessException("User does not own this item");
        }

        var resultBuilder = EditInventoryItemResult.builder()
                .userInventoryId(userInventory.getId());

        if (cmd.subtractQuantityBy() != null) {
            Item item = userInventory.getItem();

            userInventory.setQuantity(userInventory.getQuantity() - cmd.subtractQuantityBy());

            checkIfUserCanSellGivenQuantityOfItems(userInventory);
            int newUserMoney = userApi.editUserMoneyBy(cmd.userId(), item.getQuickSellValue() * cmd.subtractQuantityBy());

            if (userInventory.getQuantity() == 0) {
                userInventoryRepository.delete(userInventory);
            }

            resultBuilder.newUserMoney(newUserMoney)
                    .newQuantity(userInventory.getQuantity());

            if (userInventory.getQuantity() == 0) {
                return resultBuilder.build();
            }
        }

        if (userInventory.getIsEquipped() != cmd.isEquipped()) {
            userInventory.setIsEquipped(cmd.isEquipped());
            resultBuilder.newIsEquipped(cmd.isEquipped());
        }

        userInventoryRepository.save(userInventory);

        return resultBuilder.build();
    }

    private void checkIfUserExists(UUID userId) {
        userApi.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }

    private UserInventory getUserInventoryWithItem(UUID userInventoryId) {
        return userInventoryRepository.findWithItemById(userInventoryId)
                .orElseThrow(() -> new InventoryItemNotFound("Item in inventory not found"));
    }
}
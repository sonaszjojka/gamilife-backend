package pl.gamilife.gamification.application.usecase.editinventoryitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.exception.ForbiddenItemAccessException;
import pl.gamilife.gamification.domain.exception.InventoryItemNotFound;
import pl.gamilife.gamification.domain.model.UserInventoryItem;
import pl.gamilife.gamification.domain.port.context.UserContext;
import pl.gamilife.gamification.domain.port.repository.UserInventoryItemRepository;
import pl.gamilife.infrastructure.core.exception.domain.UserNotFoundException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class EditInventoryItemUseCaseImpl implements EditInventoryItemUseCase {

    private final UserInventoryItemRepository inventoryItemRepository;
    private final UserContext userContext;

    @Override
    public EditInventoryItemResult execute(EditInventoryItemCommand cmd) {
        checkIfUserExists(cmd.userId());
        UserInventoryItem userInventoryItem = getUserInventoryItemWithItem(cmd.userInventoryItemId());

        if (!userInventoryItem.doesBelongTo(cmd.userId())) {
            throw new ForbiddenItemAccessException("User does not own this item");
        }

        var resultBuilder = EditInventoryItemResult.builder()
                .userInventoryItemId(userInventoryItem.getId());

        if (cmd.numberOfSoldItems() != null) {
            int quickSellValue = userInventoryItem.quickSellItems(cmd.numberOfSoldItems());
            int newUserMoney = userContext.refundUserAfterQuickSell(cmd.userId(), quickSellValue);

            resultBuilder.newUserMoney(newUserMoney)
                    .newQuantity(userInventoryItem.getQuantity());

            if (userInventoryItem.getQuantity() == 0) {
                inventoryItemRepository.delete(userInventoryItem);
                return resultBuilder.userInventoryItemId(null)
                        .build();
            }
        }

        if (userInventoryItem.changeEquippedStatus(cmd.isEquipped())) {
            resultBuilder.newIsEquipped(cmd.isEquipped());
        }

        inventoryItemRepository.save(userInventoryItem);

        return resultBuilder.build();
    }

    private void checkIfUserExists(UUID userId) {
        userContext.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }

    private UserInventoryItem getUserInventoryItemWithItem(UUID userInventoryItemId) {
        return inventoryItemRepository.findWithItemById(userInventoryItemId)
                .orElseThrow(() -> new InventoryItemNotFound("Item in inventory not found"));
    }
}
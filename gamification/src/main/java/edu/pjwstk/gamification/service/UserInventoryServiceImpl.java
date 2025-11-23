package edu.pjwstk.gamification.service;

import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventoryItem;
import edu.pjwstk.gamification.repository.UserInventoryItemRepository;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Service
@AllArgsConstructor
public class UserInventoryServiceImpl implements UserInventoryService {

    private final UserInventoryItemRepository userInventoryItemRepository;

    @Override
    public UserInventoryItem addItemToUsersInventory(UUID userId, Item item) {
        Optional<UserInventoryItem> usersInventoryOptional = getUserInventoryItem(userId, item);
        UserInventoryItem userInventoryItem;
        if (usersInventoryOptional.isEmpty() || usersInventoryOptional.get().getQuantity() == null) {
            userInventoryItem = addNewItemToInventory(userId, item);
        } else {
            userInventoryItem = usersInventoryOptional.get();
            userInventoryItem.setQuantity(userInventoryItem.getQuantity() + 1);
            userInventoryItemRepository.save(userInventoryItem);
        }

        return userInventoryItem;
    }

    @Override
    @Transactional
    public void addItemsToUsersInventory(UUID userId, Set<Item> items) {
        for (Item i : items) {
            addItemToUsersInventory(userId, i);
        }
    }

    private Optional<UserInventoryItem> getUserInventoryItem(UUID userId, Item item) {
        return userInventoryItemRepository.findByUserIdAndItem(userId, item);
    }

    private UserInventoryItem addNewItemToInventory(UUID userId, Item item) {
        UserInventoryItem userInventoryItem = UserInventoryItem.builder()
                .userId(userId)
                .item(item)
                .quantity(1)
                .build();

        return userInventoryItemRepository.save(userInventoryItem);
    }
}

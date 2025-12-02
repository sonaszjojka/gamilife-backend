package pl.gamilife.gamification.service.impl;

import pl.gamilife.gamification.model.Item;
import pl.gamilife.gamification.model.UserInventoryItem;
import pl.gamilife.gamification.repository.UserInventoryItemRepository;
import pl.gamilife.gamification.service.UserInventoryService;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

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
        if (items == null || items.isEmpty()) {
            return;
        }

        List<UserInventoryItem> existingInventoryItems = userInventoryItemRepository.findAllByUserIdAndItemIn(userId, items);

        Map<Item, UserInventoryItem> inventoryMap = existingInventoryItems.stream()
                .collect(Collectors.toMap(UserInventoryItem::getItem, uii -> uii));

        List<UserInventoryItem> toSave = new ArrayList<>();
        for (Item item : items) {
            if (inventoryMap.containsKey(item)) {
                UserInventoryItem existing = inventoryMap.get(item);
                existing.setQuantity(existing.getQuantity() + 1);
                toSave.add(existing);
            } else {
                UserInventoryItem newItem = UserInventoryItem.builder()
                        .userId(userId)
                        .item(item)
                        .quantity(1)
                        .build();
                toSave.add(newItem);
            }
        }

        userInventoryItemRepository.saveAll(toSave);
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

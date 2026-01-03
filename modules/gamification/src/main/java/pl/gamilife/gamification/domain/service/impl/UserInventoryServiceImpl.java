package pl.gamilife.gamification.domain.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.UserInventoryItem;
import pl.gamilife.gamification.domain.port.repository.UserInventoryItemRepository;
import pl.gamilife.gamification.domain.service.UserInventoryService;
import pl.gamilife.shared.kernel.event.ItemAcquiredEvent;

import java.util.*;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
public class UserInventoryServiceImpl implements UserInventoryService {

    private final UserInventoryItemRepository userInventoryItemRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public UserInventoryItem addItemToUsersInventory(UUID userId, Item item) {
        Optional<UserInventoryItem> usersInventoryOptional = getUserInventoryItem(userId, item);
        UserInventoryItem userInventoryItem;

        if (usersInventoryOptional.isEmpty() || usersInventoryOptional.get().getQuantity() == null) {
            userInventoryItem = addNewItemToInventory(userId, item);
        } else {
            userInventoryItem = usersInventoryOptional.get();
            userInventoryItem.incrementQuantityBy(1);
            userInventoryItemRepository.save(userInventoryItem);
        }

        eventPublisher.publishEvent(new ItemAcquiredEvent(userId, List.of(item.getName())));
        return userInventoryItem;
    }

    @Override
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
                existing.incrementQuantityBy(1);
                toSave.add(existing);
            } else {
                UserInventoryItem newItem = UserInventoryItem.create(userId, item, 1);
                toSave.add(newItem);
            }
        }

        eventPublisher.publishEvent(new ItemAcquiredEvent(userId, items.stream().map(Item::getName).toList()));
        userInventoryItemRepository.saveAll(toSave);
    }

    private Optional<UserInventoryItem> getUserInventoryItem(UUID userId, Item item) {
        return userInventoryItemRepository.findByUserIdAndItem(userId, item);
    }

    private UserInventoryItem addNewItemToInventory(UUID userId, Item item) {
        UserInventoryItem userInventoryItem = UserInventoryItem.create(userId, item, 1);

        return userInventoryItemRepository.save(userInventoryItem);
    }
}

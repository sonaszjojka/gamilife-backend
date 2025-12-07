package pl.gamilife.gamification.domain.service;

import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.UserInventoryItem;

import java.util.Set;
import java.util.UUID;

public interface UserInventoryService {
    UserInventoryItem addItemToUsersInventory(UUID userId, Item item);

    void addItemsToUsersInventory(UUID userId, Set<Item> items);
}

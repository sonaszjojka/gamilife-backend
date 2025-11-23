package edu.pjwstk.gamification.service;

import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventoryItem;

import java.util.Set;
import java.util.UUID;

public interface UserInventoryService {
    UserInventoryItem addItemToUsersInventory(UUID userId, Item item);

    void addItemsToUsersInventory(UUID userId, Set<Item> items);
}

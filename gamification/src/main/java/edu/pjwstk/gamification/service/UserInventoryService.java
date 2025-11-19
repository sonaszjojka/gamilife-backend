package edu.pjwstk.gamification.service;

import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventoryItem;

public interface UserInventoryService {
    UserInventoryItem addItemToInventory(BasicUserInfoApiDto userDto, Item item);
}

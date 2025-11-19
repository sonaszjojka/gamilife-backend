package edu.pjwstk.gamification.service;

import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventory;

public interface UserInventoryService {
    UserInventory addItemToInventory(BasicUserInfoApiDto userDto, Item item);
}

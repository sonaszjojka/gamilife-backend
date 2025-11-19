package edu.pjwstk.gamification.service;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventory;
import edu.pjwstk.gamification.repository.ItemRepository;
import edu.pjwstk.gamification.repository.UserInventoryRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
public class UserInventoryServiceImpl implements UserInventoryService {

    private final UserApi userApi;
    private final ItemRepository itemRepository;
    private final UserInventoryRepository userInventoryRepository;

    @Override
    public UserInventory addItemToInventory(BasicUserInfoApiDto userDto, Item item) {
        Optional<UserInventory> usersInventoryOptional = getUserInventory(userDto.userId(), item);
        UserInventory userInventory;
        if (usersInventoryOptional.isEmpty() || usersInventoryOptional.get().getQuantity() == null) {
            userInventory = addNewItemToInventory(userDto.userId(), item);
        } else {
            userInventory = usersInventoryOptional.get();
            userInventory.setQuantity(userInventory.getQuantity() + 1);
            userInventoryRepository.save(userInventory);
        }

        return userInventory;
    }

    private Optional<UserInventory> getUserInventory(UUID userId, Item item) {
        return userInventoryRepository.findByUserIdAndItem(userId, item);
    }

    private UserInventory addNewItemToInventory(UUID userId, Item item) {
        UserInventory userInventory = UserInventory.builder()
                .userId(userId)
                .item(item)
                .quantity(1)
                .build();

        return userInventoryRepository.save(userInventory);
    }
}

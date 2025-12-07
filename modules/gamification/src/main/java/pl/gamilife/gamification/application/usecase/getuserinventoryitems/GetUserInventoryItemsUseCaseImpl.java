package pl.gamilife.gamification.application.usecase.getuserinventoryitems;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.UserInventoryItem;
import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;
import pl.gamilife.gamification.domain.model.filter.UserInventoryItemFilter;
import pl.gamilife.gamification.domain.port.repository.UserInventoryItemRepository;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.Comparator;
import java.util.List;
import java.util.UUID;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
@Slf4j
public class GetUserInventoryItemsUseCaseImpl implements GetUserInventoryItemsUseCase {

    private final UserInventoryItemRepository userInventoryItemRepository;

    @Override
    public GetUserInventoryItemsResult execute(GetUserInventoryItemsCommand cmd) {
        log.debug("Fetching user inventory items with filters: {}", cmd);

        ItemSlotEnum itemSlotEnum = cmd.itemSlot() != null
                ? ItemSlotEnum.fromId(cmd.itemSlot())
                : null;

        RarityEnum rarityEnum = cmd.rarity() != null
                ? RarityEnum.fromId(cmd.rarity())
                : null;

        Page<UserInventoryItem> itemPage = userInventoryItemRepository.findAll(
                new UserInventoryItemFilter(cmd.userId(), cmd.itemName(), itemSlotEnum, rarityEnum),
                cmd.page(),
                cmd.size()
        );

        List<UUID> itemIds = itemPage.map(UserInventoryItem::getId).content();

        List<UserInventoryItem> itemsWithDetails;
        if (!itemIds.isEmpty()) {
            itemsWithDetails = userInventoryItemRepository.findWithItemDetailsByIdIn(itemIds);
            itemsWithDetails.sort(Comparator.comparingInt(item -> itemIds.indexOf(item.getId())));
        } else {
            itemsWithDetails = List.of();
        }

        log.debug("Found {} user inventory items", itemPage.totalElements());

        return buildGetUserInventoryItemsResult(itemPage, itemsWithDetails);
    }

    private GetUserInventoryItemsResult buildGetUserInventoryItemsResult(
            Page<UserInventoryItem> itemPage,
            List<UserInventoryItem> items
    ) {
        return new GetUserInventoryItemsResult(
                itemPage.totalElements(),
                itemPage.totalPages(),
                itemPage.number(),
                itemPage.size(),
                items.stream().map(this::toDto).toList()
        );
    }

    private GetUserInventoryItemsResult.UserInventoryItemDto toDto(UserInventoryItem inventoryItem) {
        Item item = inventoryItem.getItem();

        return new GetUserInventoryItemsResult.UserInventoryItemDto(
                inventoryItem.getId(),
                inventoryItem.getItemId(),
                inventoryItem.getUserId(),
                inventoryItem.getQuantity(),
                inventoryItem.getIsEquipped(),
                new GetUserInventoryItemsResult.ItemDto(
                        item.getId(),
                        item.getName(),
                        item.getDescription(),
                        item.getImagePath(),
                        item.getQuickSellValue(),
                        new GetUserInventoryItemsResult.ItemSlotDto(
                                item.getItemSlot().getId(),
                                item.getItemSlot().getName()
                        ),
                        new GetUserInventoryItemsResult.RarityDto(
                                item.getRarity().getId(),
                                item.getRarity().getName()
                        ),
                        item.getPrice(),
                        item.getAchievementId(),
                        item.getUnlockLevelId()
                )
        );
    }
}
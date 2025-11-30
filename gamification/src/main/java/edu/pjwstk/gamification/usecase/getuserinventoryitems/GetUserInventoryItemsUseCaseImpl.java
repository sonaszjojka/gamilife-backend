package edu.pjwstk.gamification.usecase.getuserinventoryitems;

import edu.pjwstk.gamification.enums.ItemSlotEnum;
import edu.pjwstk.gamification.enums.RarityEnum;
import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventoryItem;
import edu.pjwstk.gamification.repository.UserInventoryItemRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;
import java.util.UUID;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
@Slf4j
public class GetUserInventoryItemsUseCaseImpl implements GetUserInventoryItemsUseCase {

    private final UserInventoryItemRepository userInventoryItemRepository;
    private final UserInventoryItemSpecificationBuilder specificationBuilder;

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
                getUserInventoryItemSpecification(cmd, itemSlotEnum, rarityEnum),
                createPageable(cmd)
        );

        List<UUID> itemIds = itemPage.map(UserInventoryItem::getId).getContent();

        List<UserInventoryItem> itemsWithDetails;
        if (!itemIds.isEmpty()) {
            itemsWithDetails = userInventoryItemRepository.findWithItemDetailsByIdIn(itemIds);
            itemsWithDetails.sort(Comparator.comparingInt(item -> itemIds.indexOf(item.getId())));
        } else {
            itemsWithDetails = List.of();
        }

        log.debug("Found {} user inventory items", itemPage.getTotalElements());

        return buildGetUserInventoryItemsResult(itemPage, itemsWithDetails);
    }

    private Specification<UserInventoryItem> getUserInventoryItemSpecification(
            GetUserInventoryItemsCommand cmd,
            ItemSlotEnum itemSlotEnum,
            RarityEnum rarityEnum
    ) {
        return specificationBuilder.buildSpecification(
                cmd.userId(),
                cmd.itemName(),
                itemSlotEnum,
                rarityEnum
        );
    }

    private Pageable createPageable(GetUserInventoryItemsCommand cmd) {
        return PageRequest.of(
                cmd.page(),
                cmd.size()
        );
    }

    private GetUserInventoryItemsResult buildGetUserInventoryItemsResult(
            Page<UserInventoryItem> itemPage,
            List<UserInventoryItem> items
    ) {
        return new GetUserInventoryItemsResult(
                itemPage.getTotalElements(),
                itemPage.getTotalPages(),
                itemPage.getNumber(),
                itemPage.getSize(),
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
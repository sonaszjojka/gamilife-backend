package edu.pjwstk.gamification.usecase.getuserinventoryitems;

import edu.pjwstk.gamification.enums.ItemSlotEnum;
import edu.pjwstk.gamification.enums.RarityEnum;
import edu.pjwstk.gamification.model.UserInventoryItem;
import org.springframework.data.jpa.domain.Specification;

import java.util.UUID;

public interface UserInventoryItemSpecificationBuilder {
    Specification<UserInventoryItem> buildSpecification(
            UUID userId,
            String itemName,
            ItemSlotEnum itemSlot,
            RarityEnum rarity
    );
}

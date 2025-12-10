package pl.gamilife.gamification.infrastructure.persistence.specification;

import jakarta.persistence.criteria.Join;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;
import pl.gamilife.gamification.domain.model.filter.StoreItemsFilter;

@Component
public class StoreItemSpecificationBuilder {

    public Specification<Item> build(
            StoreItemsFilter filter
    ){
        return Specification.allOf(
                hasItemName(filter.itemName()),
                hasItemSlot(filter.itemSlot()),
                hasRarity(filter.rarity()),
                isStoreItem()
        );
    }

    public Specification<Item> hasItemName(String itemName) {
        return (root, query, cb) ->
        {
            if (itemName == null || itemName.isBlank())
            {
                return null;
            }
            String searchPattern = "%" + itemName.trim().toLowerCase() + "%";
            return cb.like(cb.lower(root.get("name")), searchPattern);

        };
    }

   public Specification<Item> hasItemSlot(ItemSlotEnum itemSlot) {
        return (root, query, cb) ->
        {
            if (itemSlot == null) {
                return null;
            }
            return cb.equal(root.get("itemSlotId"), itemSlot.getItemSlotId());
        };
   }

   public Specification<Item> hasRarity(RarityEnum rarity) {
        return (root, query, cb) ->
        {
            if (rarity == null) {
                return null;
            }
            return cb.equal(root.get("rarityId"), rarity.getRarityId());
        };
   }

   public Specification<Item> isStoreItem()
   {
       return (root, query, cb) ->
               cb.isNotNull(root.get("price"));
   }

}

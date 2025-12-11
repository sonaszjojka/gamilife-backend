package pl.gamilife.gamification.infrastructure.persistence.specification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;
import pl.gamilife.gamification.domain.model.filter.StoreItemsFilter;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

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

   public Specification<Item> hasItemSlot(ItemSlotEnum[] itemSlot) {
       return (root, query, cb) -> {
           if (itemSlot == null || itemSlot.length == 0) {
               return null;
           }
               List<Integer> slotIds = Arrays.stream(itemSlot)
                       .map(ItemSlotEnum::getItemSlotId)
                       .collect(Collectors.toList());


           return root.get("itemSlotId").in(slotIds);
       };
   }

   public Specification<Item> hasRarity(RarityEnum[] rarity) {
        return (root, query, cb) ->
        {
            if (rarity == null || rarity.length==0) {
                return null;
            }

            List<Integer> rarityIds =  Arrays.stream(rarity)
                    .map(RarityEnum::getRarityId)
                    .collect(Collectors.toList());

            return  root.get("rarityId").in(rarityIds);
        };
   }

   public Specification<Item> isStoreItem()
   {
       return (root, query, cb) ->
               cb.isNotNull(root.get("price"));
   }

}

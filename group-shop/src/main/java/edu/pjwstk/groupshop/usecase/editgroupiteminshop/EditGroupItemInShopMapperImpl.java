package edu.pjwstk.groupshop.usecase.editgroupiteminshop;


import edu.pjwstk.groupshop.entity.GroupItemInShop;
import org.springframework.stereotype.Component;


@Component
public class EditGroupItemInShopMapperImpl implements EditGroupItemInShopMapper {
    @Override
    public EditGroupItemInShopResponse toResponse( GroupItemInShop groupItemInShop ) {

        return EditGroupItemInShopResponse.builder()
                .groupItemInShopId(groupItemInShop.getGroupItemId())
                .price(groupItemInShop.getPrice())
                .name(groupItemInShop.getName())
                .isActive(groupItemInShop.getIsActive())
                .createdAt(groupItemInShop.getCreatedAt())
                .groupShopId(groupItemInShop.getGroupShop().getGroupShopId())
                .build();
    }
}

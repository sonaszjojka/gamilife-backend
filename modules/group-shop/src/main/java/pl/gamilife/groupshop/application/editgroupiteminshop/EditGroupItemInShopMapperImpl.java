package pl.gamilife.groupshop.application.editgroupiteminshop;


import org.springframework.stereotype.Component;
import pl.gamilife.groupshop.domain.model.GroupItemInShop;


@Component
public class EditGroupItemInShopMapperImpl implements EditGroupItemInShopMapper {
    @Override
    public EditGroupItemInShopResponse toResponse(GroupItemInShop groupItemInShop) {

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

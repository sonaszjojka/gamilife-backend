package pl.gamilife.groupshop.usecase.creategroupiteminshop;

import org.springframework.stereotype.Component;
import pl.gamilife.groupshop.entity.GroupItemInShop;
import pl.gamilife.groupshop.entity.GroupShop;

import java.time.Instant;
import java.util.UUID;

@Component
public class CreateGroupItemInShopMapperImpl implements CreateGroupItemInShopMapper {


    @Override
    public GroupItemInShop toEntity(CreateGroupItemInShopRequest request, GroupShop groupShop, UUID groupItemId) {
        return GroupItemInShop.builder()
                .groupItemId(groupItemId)
                .price(request.price())
                .name(request.name())
                .createdAt(Instant.now())
                .isActive(request.isActive())
                .groupShop(groupShop)
                .build();
    }

    @Override
    public CreateGroupItemInShopResponse toResponse(GroupItemInShop groupItemInShop) {
        return CreateGroupItemInShopResponse.builder()
                .groupItemId(groupItemInShop.getGroupItemId())
                .price(groupItemInShop.getPrice())
                .name(groupItemInShop.getName())
                .createdAt(groupItemInShop.getCreatedAt())
                .isActive(groupItemInShop.getIsActive())
                .groupShopId(groupItemInShop.getGroupShop().getGroupShopId())
                .build();
    }
}

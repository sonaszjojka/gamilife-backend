package edu.pjwstk.groupshop.usecase.createownedgroupitem;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.groupshop.entity.GroupItemInShop;
import edu.pjwstk.groupshop.entity.OwnedGroupItem;
import edu.pjwstk.groupshop.repository.GroupItemInShopRepository;
import edu.pjwstk.groupshop.repository.OwnedGroupItemRpository;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.UUID;

@Service
public class CreateOwnedGroupItemUseCaseImpl implements CreateOwnedGroupItemUseCase {

    private final CreateOwnedGroupItemMapper createOwnedGroupItemMapper;
    private final GroupItemInShopRepository groupItemInShopRepository;
    private final OwnedGroupItemRpository ownedGroupItemRpository;
    private final GroupApi groupProvider;
    private final AuthApi currentUserProvider;

    public CreateOwnedGroupItemUseCaseImpl(CreateOwnedGroupItemMapper createOwnedGroupItemMapper, GroupItemInShopRepository groupItemInShopRepository, OwnedGroupItemRpository ownedGroupItemRpository, GroupApi groupProvider, AuthApi currentUserProvider) {
        this.createOwnedGroupItemMapper = createOwnedGroupItemMapper;
        this.groupItemInShopRepository = groupItemInShopRepository;
        this.ownedGroupItemRpository = ownedGroupItemRpository;
        this.groupProvider = groupProvider;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    public CreateOwnedGroupItemResponse execute(CreateOwnedGroupItemRequest request, UUID groupMemberId,UUID groupId) {

        GroupDto groupDto = groupProvider.findGroupById(groupId);
        CurrentUserDto currentUser = currentUserProvider.getCurrentUser().orElseThrow();

        if (groupProvider.findGroupMemberById(groupMemberId)==null) {
            throw new RuntimeException("Group member not found in the specified group!");
        }

        if (!currentUser.userId().equals(groupDto.adminId()) &&  !currentUser.userId().equals(groupMemberId)) {
            throw new RuntimeException("Only group administrators or the member themselves can add items to inventory!");
        }


        GroupItemInShop groupItemInShop = groupItemInShopRepository.findById(request.groupItemId())
                .orElseThrow(() -> new RuntimeException("Group item in shop not found"));

        if (Boolean.FALSE.equals(groupItemInShop.getIsActive()))
        {
            throw new RuntimeException("Cannot add inactive group item to inventory!");
        }

        Instant useDate = request.isUsedUp() ? Instant.now() : null;
        OwnedGroupItem ownedGroupItem = createOwnedGroupItemMapper.toEntity(request, groupMemberId, groupItemInShop,UUID.randomUUID(),useDate);
        return createOwnedGroupItemMapper.toResponse(ownedGroupItemRpository.save(ownedGroupItem));
    }
}

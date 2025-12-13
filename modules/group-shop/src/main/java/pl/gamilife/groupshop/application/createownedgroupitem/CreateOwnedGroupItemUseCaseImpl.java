package pl.gamilife.groupshop.application.createownedgroupitem;

import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.groupshop.domain.model.GroupItemInShop;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.exception.InvalidOwnedGroupItemDataException;
import pl.gamilife.groupshop.domain.port.repository.GroupItemInShopRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.groupshop.domain.port.repository.OwnedGroupItemRpository;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

import java.time.Instant;
import java.util.UUID;

@Service
public class CreateOwnedGroupItemUseCaseImpl implements CreateOwnedGroupItemUseCase {

    private final CreateOwnedGroupItemMapper createOwnedGroupItemMapper;
    private final GroupItemInShopRepository groupItemInShopRepository;
    private final OwnedGroupItemRpository ownedGroupItemRpository;
    private final GroupShopRepository groupShopRepository;
    private final GroupApi groupProvider;
    private final AuthApi currentUserProvider;

    public CreateOwnedGroupItemUseCaseImpl(CreateOwnedGroupItemMapper createOwnedGroupItemMapper, GroupItemInShopRepository groupItemInShopRepository, OwnedGroupItemRpository ownedGroupItemRpository, GroupShopRepository groupShopRepository, GroupApi groupProvider, AuthApi currentUserProvider) {
        this.createOwnedGroupItemMapper = createOwnedGroupItemMapper;
        this.groupItemInShopRepository = groupItemInShopRepository;
        this.ownedGroupItemRpository = ownedGroupItemRpository;
        this.groupShopRepository = groupShopRepository;
        this.groupProvider = groupProvider;
        this.currentUserProvider = currentUserProvider;
    }

    @Override
    public CreateOwnedGroupItemResponse execute(CreateOwnedGroupItemRequest request, UUID groupMemberId, UUID groupId) {

        GroupDto groupDto = groupProvider.findGroupById(groupId);
        CurrentUserDto currentUser = currentUserProvider.getCurrentUser();

        if (groupProvider.findGroupMemberById(groupMemberId) == null) {
            throw new GroupMemberNotFoundException("Group member not found in the specified group!");
        }

        GroupShop groupShop = groupShopRepository.findByGroupId(groupId).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        if (!currentUser.userId().equals(groupDto.adminId()) && !currentUser.userId().equals(groupProvider.findGroupMemberById(groupMemberId).userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("Only group administrators or the member themselves can add items to inventory!");
        }


        GroupItemInShop groupItemInShop = groupItemInShopRepository.findById(request.groupItemId())
                .orElseThrow(() -> new RuntimeException("Group item in shop not found"));

        if (Boolean.FALSE.equals(groupItemInShop.getIsActive())) {
            throw new InvalidOwnedGroupItemDataException("Cannot add inactive group item to inventory!");
        }

        Instant useDate = request.isUsedUp() ? Instant.now() : null;
        OwnedGroupItem ownedGroupItem = createOwnedGroupItemMapper.toEntity(request, groupMemberId, groupItemInShop, UUID.randomUUID(), useDate);
        return createOwnedGroupItemMapper.toResponse(ownedGroupItemRpository.save(ownedGroupItem));
    }
}

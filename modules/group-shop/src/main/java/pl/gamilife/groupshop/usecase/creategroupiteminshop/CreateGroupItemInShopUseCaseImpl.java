package pl.gamilife.groupshop.usecase.creategroupiteminshop;

import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.groupshop.entity.GroupItemInShop;
import pl.gamilife.groupshop.entity.GroupShop;
import pl.gamilife.groupshop.exception.domain.GroupShopNotFoundException;
import pl.gamilife.groupshop.exception.domain.InactiveGroupShopException;
import pl.gamilife.groupshop.repository.GroupItemInShopRepository;
import pl.gamilife.groupshop.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;

import java.util.UUID;

@Service
public class CreateGroupItemInShopUseCaseImpl implements CreateGroupItemInShopUseCase {
    private final CreateGroupItemInShopMapper createGroupItemInShopMapper;
    private final GroupItemInShopRepository groupItemInShopRepository;
    private final GroupShopRepository groupShopRepository;
    private final GroupApi groupProvider;
    private final AuthApi currentUserProvider;

    public CreateGroupItemInShopUseCaseImpl(CreateGroupItemInShopMapper createGroupItemInShopMapper, GroupItemInShopRepository groupItemInShopRepository, GroupShopRepository groupShopRepository, GroupApi groupProvider, AuthApi currentUserProvider) {
        this.createGroupItemInShopMapper = createGroupItemInShopMapper;
        this.groupItemInShopRepository = groupItemInShopRepository;
        this.groupShopRepository = groupShopRepository;
        this.groupProvider = groupProvider;
        this.currentUserProvider = currentUserProvider;
    }


    @Override
    public CreateGroupItemInShopResponse execute(CreateGroupItemInShopRequest request, UUID groupId, UUID groupShopId) {

        GroupShop groupShop = groupShopRepository.findByGroupShopId(groupShopId).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop with id: " + groupShopId + " not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        GroupDto groupDto = groupProvider.findGroupById(groupId);
        if (!groupShop.getGroupId().equals(groupId)) {
            throw new GroupShopNotFoundException("Group shop with id: " + groupShopId + " does not belong to group with id: " + groupId + "!");
        }

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();

        if (!currentUserDto.userId().equals(groupDto.adminId()) && Boolean.TRUE.equals(request.isActive())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can create active group item in shop!");
        }
        GroupItemInShop groupItemInShop = createGroupItemInShopMapper.toEntity(request, groupShop, UUID.randomUUID());
        groupItemInShopRepository.save(groupItemInShop);
        return createGroupItemInShopMapper.toResponse(groupItemInShop);
    }
}

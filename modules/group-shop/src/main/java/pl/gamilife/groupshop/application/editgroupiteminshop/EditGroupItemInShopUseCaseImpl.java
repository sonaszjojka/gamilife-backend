package pl.gamilife.groupshop.application.editgroupiteminshop;

import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.exception.GroupShopItemNotFoundException;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.port.repository.GroupItemInShopRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;

import java.util.UUID;

@Service
public class EditGroupItemInShopUseCaseImpl implements EditGroupItemInShopUseCase {
    private final EditGroupItemInShopMapper editGroupItemInShopMapper;
    private final GroupItemInShopRepository groupItemInShopRepository;
    private final GroupShopRepository groupShopRepository;
    private final AuthApi currentUserProvider;
    private final GroupApi groupApi;

    public EditGroupItemInShopUseCaseImpl(EditGroupItemInShopMapper editGroupItemInShopMapper, GroupItemInShopRepository groupItemInShopRepository, GroupShopRepository groupShopRepository, AuthApi currentUserProvider, GroupApi groupApi) {
        this.editGroupItemInShopMapper = editGroupItemInShopMapper;
        this.groupItemInShopRepository = groupItemInShopRepository;
        this.groupShopRepository = groupShopRepository;
        this.currentUserProvider = currentUserProvider;
        this.groupApi = groupApi;
    }

    @Override
    public EditGroupItemInShopResponse execute(UUID groupItemId, UUID groupId, EditGroupItemInShopRequest request) {

        GroupShop groupShop = groupShopRepository.findByGroupId(groupId).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser();
        GroupDto groupDto = groupApi.findGroupById(groupId);

        if (!currentUserDto.userId().equals(groupDto.adminId()) && Boolean.TRUE.equals(request.isActive())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can make group items active!");
        }
        GroupItem groupItem = groupItemInShopRepository.findById(groupItemId).orElseThrow(
                () -> new GroupShopItemNotFoundException("Group item in shop with id: " + groupItemId + " not found!"));

        groupItem.setPrice(request.price());
        groupItem.setName(request.name());
        groupItem.setIsActive(request.isActive());

        groupItemInShopRepository.save(groupItem);
        return editGroupItemInShopMapper.toResponse(groupItem);

    }
}

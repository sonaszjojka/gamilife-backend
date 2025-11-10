package edu.pjwstk.groups.usecase.getgroups.getbyid;

import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.enums.GroupTypeEnum;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.util.GroupSpecificationBuilder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class GetGroupByIdUseCaseImpl implements GetGroupByIdUseCase {

    private final GroupJpaRepository groupRepository;

    @Override
    public GetGroupByIdResult executeInternal(GetGroupByIdCommand cmd) {
        log.debug("Fetching group by id: {}", cmd);

        Group group = groupRepository.findById(cmd.groupId())
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + cmd.groupId() + " not found!"));

        log.debug("Found group with id: {}", cmd);

        return buildGetGroupByIdResult(group);
    }

    private GetGroupByIdResult buildGetGroupByIdResult(Group group) {
        return new GetGroupByIdResult(
                group.getGroupId(),
                group.getJoinCode(),
                group.getName(),
                group.getAdminId(),
                group.getGroupCurrencySymbol(),
                group.getMembersLimit(),
                new GetGroupByIdResult.GroupTypeDto(group.getGroupType().getTitle()),
                group.getGroupMembers().size()
        );
    }
}
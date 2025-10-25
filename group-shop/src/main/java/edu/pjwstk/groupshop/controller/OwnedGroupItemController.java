package edu.pjwstk.groupshop.controller;

import edu.pjwstk.groupshop.usecase.createownedgroupitem.CreateOwnedGroupItemRequest;
import edu.pjwstk.groupshop.usecase.createownedgroupitem.CreateOwnedGroupItemResponse;
import edu.pjwstk.groupshop.usecase.createownedgroupitem.CreateOwnedGroupItemUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;


@RestController
@RequestMapping("/api/v1/groups/{groupId}/members/{memberId}/inventory")
public class OwnedGroupItemController {
    private final CreateOwnedGroupItemUseCase createOwnedGroupItemUseCase;

    public OwnedGroupItemController(CreateOwnedGroupItemUseCase createOwnedGroupItemUseCase) {
        this.createOwnedGroupItemUseCase = createOwnedGroupItemUseCase;
    }


    @PostMapping("")
    public ResponseEntity<CreateOwnedGroupItemResponse> createOwnedGroupItem(@PathVariable (name="groupId") UUID groupId,
                                                                             @PathVariable (name="memberId") UUID memberId,
                                                                             @RequestBody @Valid CreateOwnedGroupItemRequest request) {

        CreateOwnedGroupItemResponse response= createOwnedGroupItemUseCase.execute(request, memberId,groupId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response) ;
    }


}

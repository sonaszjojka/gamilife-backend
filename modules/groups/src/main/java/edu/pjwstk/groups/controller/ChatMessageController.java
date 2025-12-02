package edu.pjwstk.groups.controller;

import edu.pjwstk.groups.controller.request.CreateChatMessageRequest;
import edu.pjwstk.groups.usecase.createchatmessage.CreateChatMessageCommand;
import edu.pjwstk.groups.usecase.createchatmessage.CreateChatMessageResult;
import edu.pjwstk.groups.usecase.createchatmessage.CreateChatMessageUseCase;
import edu.pjwstk.groups.usecase.getchatmessages.GetChatMessagesCommand;
import edu.pjwstk.groups.usecase.getchatmessages.GetChatMessagesResult;
import edu.pjwstk.groups.usecase.getchatmessages.GetChatMessagesUseCase;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/groups/{groupId}/members/{groupMemberId}/chat-messages")
public class ChatMessageController {

    private final CreateChatMessageUseCase createChatMessageUseCase;
    private final GetChatMessagesUseCase getChatMessagesUseCase;


    @PostMapping
    public ResponseEntity<CreateChatMessageResult> save(@RequestBody @Valid CreateChatMessageRequest request,
                                                        @PathVariable("groupId") UUID groupId,
                                                        @PathVariable("groupMemberId") UUID groupMemberId) {
        CreateChatMessageResult response = createChatMessageUseCase.execute(new CreateChatMessageCommand(
                groupId,
                groupMemberId,
                request.content(),
                request.isImportant()
        ));
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @GetMapping
    public ResponseEntity<GetChatMessagesResult> getAllChatMessages(
            @PathVariable("groupId") UUID groupId,
            @RequestParam(required = false) Boolean isImportant,
            @RequestParam(defaultValue = "0") @Min(0) Integer page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(100) Integer size) {

        GetChatMessagesResult response = getChatMessagesUseCase.execute(
                new GetChatMessagesCommand(
                        groupId,
                        isImportant,
                        page,
                        size
                )
        );
        return ResponseEntity.ok(response);
    }
}
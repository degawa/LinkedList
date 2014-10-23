module class_DoublyList
    use class_DoublyLink
    implicit none
    private
    public :: DoublyList
    public :: DoublyListIterator

    !Doubly Linked List
    !List����̂��߂�component��type-bound procedure���`
    type :: DoublyList
        private
        class(DoublyLink),pointer :: firstLink => null() !List�̐擪�̗v�f
        class(DoublyLink),pointer :: lastLink  => null() !List�̖����̗v�f
        class(DoublyLink),pointer :: currLink  => null() !List�̌��݂̗v�f
        integer :: numberOfItem = 0
    contains
        procedure,pass :: printList               !List�̑S�v�f��\��
        procedure,pass :: print                   !printList����Ă΂��i�v�f������print()���ĂԂ����j
        
        procedure,pass :: addDoublyLinkToList     !List�̌��݈ʒu�̌��ɐV�����v�f��ǉ�
        procedure,pass :: addDoublyLinkToFirst    !List�̐擪�ɐV�����v�f��ǉ�
        procedure,pass :: addDoublyLinkToLast     !List�̖����ɐV�����v�f��ǉ�
        procedure,pass :: removeCurrentDoublyLink !List�̌��݈ʒu�̗v�f���폜�DcurrLink�͈�O��
        procedure,pass :: removeFirstDoublyLink   !List�̐擪�v�f���폜�DcurrLink�͈����
        procedure,pass :: removeLastDoublyLink    !List�̖����v�f���폜�DcurrLink�͈�O��
        procedure,pass :: getCurrentValue         !List�̌��݈ʒu�̗v�f���擾
        procedure,pass :: getValueOf              !List���̐擪����w�肵���ԍ��ڂ̗v�f���擾
        procedure,pass :: getFirstValue           !List�̐擪�̗v�f���擾
        procedure,pass :: getLastValue            !List�̖����̗v�f���擾
        procedure,pass :: isEmpty                 !List���󂩂𒲍�
        procedure,pass :: isCurrentLinkFirst      !�y�폜�\��zcurrLink���擪���ǂ����𒲍��DhasPrevLink�Ƌ@�\���d���D����`�F�b�N���s�\��
        procedure,pass :: isCurrentLinkLast       !�y�폜�\��zcurrLink���������ǂ����𒲍��DhasNextLink�Ƌ@�\���d���D����`�F�b�N���s�\��
        procedure,pass :: hasNextLink             !���ɗv�f�����邩�𒲍��DisCurrnetLinkLast�Ƌ@�\���d��
        procedure,pass :: hasPrevLink             !�O�ɗv�f�����邩�𒲍��DisCurrnetLinkFirst�Ƌ@�\���d��
        procedure,pass :: clearList               !List����ɂ���iList���̑S�v�f���폜�j
        procedure,pass :: incrementSizeOfList     !List�v�f�̌���+1
        procedure,pass :: decrementSizeOfList     !List�v�f�̌���-1
        procedure,pass :: getSizeOfList           !List�v�f�̌����擾

        generic        :: add         => addDoublyLinkToList
        generic        :: addFirst    => addDoublyLinkToFirst
        generic        :: addLast     => addDoublyLinkToLast
        generic        :: remove      => removeCurrentDoublyLink
        generic        :: removeFirst => removeFirstDoublyLink
        generic        :: removeLast  => removeLastDoublyLink
        generic        :: get         => getCurrentValue
        generic        :: getFirst    => getFirstValue
        generic        :: getLast     => getLastValue
        generic        :: clear       => clearList
        generic        :: size        => getSizeOfList
        generic        :: isFirst     => isCurrentLinkFirst
        generic        :: isLast      => isCurrentLinkLast

        procedure,  pass :: iterator        !iterator�̃R���X�g���N�^
        procedure,nopass :: destroyIterator !iterator�̃f�X�g���N�^
        
    end type DoublyList

    !Doubly Linked List�����p��iterator
    !Java��List�ɏ����āChasNext,next, remove���`
    type :: DoublyListIterator
        private
        type(DoublyList),pointer :: List => null()
        
        type(DoublyLink),pointer :: LastAccessedLink => null() !�Ō�ɃA�N�Z�X����List�̗v�f�ւ̃|�C���^�Dnext��remove�̎����ɕK�v
        type(DoublyLink),pointer ::      iterateLink => null() !List�𑖍����邽�߂̃|�C���^
    contains
        procedure,pass :: hasNext !List�Ɏ��̗v�f�����邩
        procedure,pass :: next    !List�̌��݂̗v�f��Ԃ���currLink������֐i�߂�

        procedure,pass         :: moveToFirst             !Iterator��List�擪�ցiJava�̂悤�ȃK�x�b�W�R���N�V�������Ȃ����߁Citerator���ė��p����j
        procedure,pass         :: moveToLast              !�y�폜�\��zIterator��List�����ցi�g���ǂ��낪�s���j
        procedure,pass         :: removeObtainedItem      !next�Ŏ擾�����v�f��List����폜
        procedure,pass,private :: removeObtainedFirstItem !removeObtainedItem����Ă΂��Dnext�Ŏ擾�����v�f���擪�̏ꍇ�ɗ��p
        procedure,pass,private :: removeObtainedLastItem  !removeObtainedItem����Ă΂��Dnext�Ŏ擾�����v�f���擪�̏ꍇ�ɗ��p
        
        generic        :: first       => moveToFirst
        generic        :: last        => moveToLast
        generic        :: remove      => removeObtainedItem
        generic        :: removeFirst => removeObtainedFirstItem
        generic        :: removeLast  => removeObtainedLastItem

    end type DoublyListIterator
    
    contains
    
!---------------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !Iterator�p�w���p�֐��Q
    !-----------------------------------------------------------------------------------------!
    !�R���X�g���N�^
    function iterator(this)
        implicit none
        class(DoublyList),target :: this
        type(DoublyListIterator),pointer :: iterator
        
        allocate(DoublyListIterator::iterator)
        iterator%List => this
        call iterator%first()
    end function iterator
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !�f�X�g���N�^
    subroutine destroyIterator(iterator)
        implicit none
        type(DoublyListIterator),pointer :: iterator
        
        deallocate(iterator)

    end subroutine destroyIterator
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List�Ɏ��̗v�f�����邩�𒲍�
    !do-while�̌p�������ɗ��p
    function hasNext(this)
        implicit none
        class(DoublyListIterator) :: this
        logical hasNext
        
        if( associated(this%List%currLink) )then
            hasNext = .true.
        else
            hasNext = .false.
        end if

    end function hasNext
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List�̌��݂̗v�f��Ԃ���currLink������֐i�߂�
    function next(this)
        implicit none
        class(DoublyListIterator) :: this
        class(*),pointer :: next

        if(associated(this%List%currLink))then
            this%LastAccessedLink => this%List%currLink
            next => this%List%currLink
            this%List%currLink=>this%List%currLink%getNextLink()
        else
            next => null()
        end if
        
    end function next
    !-----------------------------------------------------------------------------------------!

    !-----------------------------------------------------------------------------------------!
    !remove�p�֐��Q
    !-----------------------------------------------------------------------------------------!
    !next�Ŏ擾�����v�f��List����폜
    subroutine removeObtainedItem(this)
        implicit none
        class(DoublyListIterator) :: this
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%List%isEmpty() )then
            stop 'empty list'
        else
            if( .not.associated(this%LastAccessedLink%getNextLink()) )then !current is last
                !print *,"remove last items"
                call this%removeLast()
            else&
            if( .not.associated(this%LastAccessedLink%getPrevLink()) )then !current is first
                !print *,"remove first items"
                call this%removeFirst()
            else                                                   !elsewhere
                !print *,"remove items"
                prevLink => this%LastAccessedLink%getPrevLink()
                nextLink => this%LastAccessedLink%getNextLink()
                call prevLink%setNextLink(nextLink)
                call nextLink%setPrevLink(prevLink)
                deallocate(this%lastAccessedLink)
                this%List%currLink => nextLink
                prevLink => null()
                nextLink => null()
                this%LastAccessedLink => this%List%currLink%getPrevLink()
                call this%List%decrementSizeOfList()
                !print *,"insert"
            end if
        end if
        
    end subroutine removeObtainedItem
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !next�Ŏ擾�����v�f��List����폜
    !removeObtainedItem����Ă΂��Dnext�Ŏ擾�����v�f���擪�̏ꍇ�ɗ��p
    subroutine removeObtainedFirstItem(this)
        implicit none
        class(DoublyListIterator) :: this
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%List%isEmpty() )then
            stop 'empty list'
        else
            prevLink => this%LastAccessedLink%getPrevLink()
            nextLink => this%LastAccessedLink%getNextLink()

            call nextLink%setPrevLink(prevLink)
            deallocate(this%lastAccessedLink)
            this%List%currLink => nextLink
            this%List%firstLink => this%List%currLink
            prevLink => null()
            nextLink => null()
            this%LastAccessedLink => this%List%currLink
            call this%List%decrementSizeOfList()
        end if
        
    end subroutine removeObtainedFirstItem
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !next�Ŏ擾�����v�f��List����폜
    !removeObtainedItem����Ă΂��Dnext�Ŏ擾�����v�f���擪�̏ꍇ�ɗ��p
    subroutine removeObtainedLastItem(this)
        implicit none
        class(DoublyListIterator) :: this
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%List%isEmpty() )then
            stop 'empty list'
        else
            prevLink => this%LastAccessedLink%getPrevLink()
            nextLink => this%LastAccessedLink%getNextLink()
            call prevLink%setNextLink(nextLink)

            deallocate(this%lastAccessedLink)
            this%List%lastLink    => prevLink
            this%LastAccessedLink => prevLink
            this%iterateLink      => null()
            this%List%currLink    => null() !iterator�ł�next()�ň���current�������
            prevLink => null()
            nextLink => null()
            call this%List%decrementSizeOfList()
        end if
        
    end subroutine removeObtainedLastItem
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    subroutine moveToFirst(this)
        implicit none
        class(DoublyListIterator) :: this
        
        this%List%currLink => this%List%firstLink
    end subroutine moveToFirst
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    subroutine moveToLast(this)
        implicit none
        class(DoublyListIterator) :: this
        
        this%List%currLink => this%List%lastLink
    end subroutine moveToLast
    !-----------------------------------------------------------------------------------------!
    
!-------------------------doubly list�p type-bound procedure----------------------------------!
    !-----------------------------------------------------------------------------------------!
    !�w���p�֐��Q
    !-----------------------------------------------------------------------------------------!
    !List���󂩂𒲍�
    function isEmpty(this)
        implicit none
        class(DoublyList) :: this
        logical :: isEmpty
        
        if(associated(this%firstLink))then
            isEmpty = .false.
        else
            isEmpty = .true.
        end if
    end function isEmpty
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !�y�폜�\��zcurrLink���擪���ǂ����𒲍��DhasPrevLink�Ƌ@�\���d���D����`�F�b�N���s�\��
    function isCurrentLinkFirst(this) result(isFirst)
        use iso_c_binding
        implicit none
        
        class(DoublyList) :: this
        logical :: isFirst
        
        type(c_ptr) :: ptr_curr, ptr_first
        
        ptr_curr  = c_loc(this%currLink ); call c_f_pointer(ptr_curr, this%currLink)
        ptr_first = c_loc(this%firstLink); call c_f_pointer(ptr_first, this%firstLink)
        !print *,c_associated(ptr_curr,ptr_first)
        
        if(c_associated(ptr_curr,ptr_first))then
            isFirst = .true.
        else
            isFirst = .false.
        end if
        
    end function isCurrentLinkFirst
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !�y�폜�\��zcurrLink���������ǂ����𒲍��DhasNextLink�Ƌ@�\���d���D����`�F�b�N���s�\��
    function isCurrentLinkLast(this) result(isLast)
        use iso_c_binding
        implicit none
        
        class(DoublyList) :: this
        logical :: isLast
        
        type(c_ptr) :: ptr_curr, ptr_last
        
        ptr_curr = c_loc(this%currLink); call c_f_pointer(ptr_curr, this%currLink)
        ptr_last = c_loc(this%LastLink); call c_f_pointer(ptr_last, this%lastLink)
        !print *,c_associated(ptr_curr,ptr_first)
        
        if(c_associated(ptr_curr,ptr_last))then
            isLast = .true.
        else
            isLast = .false.
        end if
        
    end function isCurrentLinkLast
    !-----------------------------------------------------------------------------------------!
    !List�𑖍����邽�߂̊֐�
    !-----------------------------------------------------------------------------------------!
    !���ɗv�f�����邩�𒲍��DisCurrnetLinkLast�Ƌ@�\���d��
    function hasNextLink(this)
        implicit none
        class(DoublyList) :: this
        logical :: hasNextLink
        
        if(associated(this%currLink%getNextLink()))then
            hasNextLink = .true.
        else
            hasNextLink = .false.
        end if
    end function hasNextLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !�O�ɗv�f�����邩�𒲍��DisCurrnetLinkFirst�Ƌ@�\���d��
    function hasPrevLink(this)
        implicit none
        class(DoublyList) :: this
        logical :: hasPrevLink
        
        if(associated(this%currLink%getPrevLink()))then
            hasPrevLink = .true.
        else
            hasPrevLink = .false.
        end if
    end function hasPrevLink
    !-----------------------------------------------------------------------------------------!
    
    !-----------------------------------------------------------------------------------------!
    !��ʕ\���p�֐��D����m�F�p�ɍ쐬�D���p����ɈӋ`���������Ȃ��̂ō폜�𐄏�
    !-----------------------------------------------------------------------------------------!
    !List�̑S�v�f��\��
    subroutine print(this)
        class(DoublyList) :: this
        
        if(.not.this%isEmpty())then
            call this%currLink%print()
        end if
    end subroutine print
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !printList����Ă΂��i�v�f������print()���ĂԂ����j
    subroutine printList(this)
        class(Doublylist) :: this
        class(DoublyLink),pointer :: doublyLinkItem
        !class(*),pointer :: item !select type�p

        type(DoublyListIterator),pointer :: itr
    
        if(this%isEmpty())then
            print *,"empty list"
            return
        end if
        
        itr => this%iterator()
        do while(itr%hasNext())
            !next()��class(*)��Ԃ��̂�select type�œK�؂Ȍ^�idoublyLink�j��I�ԕK�v������
            !item=>itr%next()
            !select type(item)
            !class is(DoublyLink)
            !    doublyLinkItem => item
            !    call doublyLinkItem%print()
            !end select
            !�������͋[���I�ɃL���X�g���s���֐����g��
            doublyLinkItem => toDoublyLink(itr%next())
            call doublyLinkItem%print()
        end do
        call this%destroyIterator(itr)
        
    end subroutine printList
    !-----------------------------------------------------------------------------------------!
    !List�̗v�f��
    !-----------------------------------------------------------------------------------------!
    !List�v�f����+1
    subroutine incrementSizeOfList(this)
        class(Doublylist) :: this
        
        this%numberOfItem = this%numberOfItem + 1
    end subroutine incrementSizeOfList
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List�v�f����-1
    subroutine decrementSizeOfList(this)
        class(Doublylist) :: this
        
        this%numberOfItem = this%numberOfItem - 1
    end subroutine decrementSizeOfList
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List�v�f�����擾
    integer function getSizeOfList(this)
        class(Doublylist) :: this
        
        getSizeOfList = this%numberOfItem
    end function getSizeOfList
    !-----------------------------------------------------------------------------------------!

    !-----------------------------------------------------------------------------------------!
    !List�֗v�f��ǉ�
    !-----------------------------------------------------------------------------------------!
    !List�̌��݈ʒu�̌��ɐV�����v�f��ǉ�
    subroutine addDoublyLinkToList(this,value)
        implicit none
        class(DoublyList),intent(inout) :: this
        class(*),intent(in)    :: value
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%isEmpty() )then                      !List����
            call this%addFirst(value)
            !print *,"create link and add to first"
        else if( .not.associated(this%currLink) )then !�J�����g�v�f��Null�̎��͏I�[�Ɣ��f
            call this%addLast(value)
        else                                          !List����łȂ����
            !�O�ƌ�낪�󂢂Ă���΁C���ɒǉ�
            if( .not.this%hasNextLink() )then         !current��Last�Ȃ疖���ɒǉ�
                !print *,"add to last"
                call this%addLast(value)
            else
                !print *,"insert"
                prevLink => this%currLink             !�ǉ����郊���N�ɂƂ��đO�v�f�Ƃ̓J�����g�v�f
                nextLink => this%currLink%getNextLink()
                this%currLink => new_DoublyLink(value, prevLink, nextLink)
                call prevLink%setNextLink(this%currLink)
                call nextLink%setPrevLink(this%currLink)
                prevLink => null()
                nextLink => null()
                if( .not.associated(this%currLink%getNextLink()) ) this%lastLink => this%currLink
                call this%incrementSizeOfList()
            end if
        end if

    end subroutine addDoublyLinkToList
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List�̐擪�ɐV�����v�f��ǉ�
    subroutine addDoublyLinkToFirst(this,value)
        implicit none
        class(DoublyList),intent(inout) :: this
        class(*),intent(in)    :: value
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
            prevLink => null()
        if( this%isEmpty() )then !List����
            nextLink => null()
            this%firstLink => new_DoublyLink(value, prevLink, nextLink)
            this% lastLink => this%firstLink
        else
            nextLink => this%firstLink
            this%firstLink => new_DoublyLink(value, prevLink, nextLink)
            nextLink => this%firstLink%getNextLink()
            call nextLink%setPrevLink(this%firstLink)
            nextLink => null()
        end if
            nextLink => null()
            prevLink => null()
        
        this% currLink => this%firstLink !if necessary
        call this%incrementSizeOfList()

    end subroutine addDoublyLinkToFirst
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List�̖����ɐV�����v�f��ǉ�
    subroutine addDoublyLinkToLast(this,value)
        implicit none
        class(DoublyList),intent(inout) :: this
        class(*),intent(in)    :: value
        
        class(DoublyLink),pointer ::  newLink
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%isEmpty() )then !List����
            call this%addFirst(value)
        else
            prevLink => this%lastLink
            nextLink => null()
             newLink => new_DoublyLink(value, prevLink, nextLink)
            prevLink => newLink%getPrevLink()
            call prevLink%setNextLink(newLink)
            this%lastLink => newLink
            call this%incrementSizeOfList()
        end if
            this%currLink => this%LastLink !if necessary
            nextLink => null()
            prevLink => null()
             newLink => null()
    
    end subroutine addDoublyLinkToLast
    !-----------------------------------------------------------------------------------------!
    
    !-----------------------------------------------------------------------------------------!
    !List����v�f���폜
    !known bug �����̊֐���List����ɂ���ƃA�N�Z�X�ᔽ�������DList����ɂ���Ƃ���clear���g�p
    !-----------------------------------------------------------------------------------------!
    !List�̌��݈ʒu�̗v�f���폜�DcurrLink�͈�O��
    subroutine removeCurrentDoublyLink(this)
        implicit none
        class(DoublyList),intent(inout) :: this
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%isEmpty() )then
            stop 'empty list'
        else
            if( .not.this%hasNextLink() )then !current�������Ȃ�
                call this%removeLast()
            else&
            if( .not.this%hasPrevLink() )then !current���擪�Ȃ�
                call this%removeFirst()
            else                              !����ȊO
                prevLink => this%currLink%getPrevLink()
                nextLink => this%currLink%getNextLink()
                deallocate(this%currLink)
                this%currLink => prevLink
                call this%currLink%setNextLink(nextLink)
                call      nextLink%setPrevLink(this%currLink)
                prevLink => null()
                nextLink => null()

                call this%decrementSizeOfList()
            end if
        end if
        
    end subroutine removeCurrentDoublyLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List�̐擪�v�f���폜�DcurrLink�͈����
    subroutine removeFirstDoublyLink(this)
        implicit none
        class(DoublyList),intent(inout) :: this
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%isEmpty() )then
            stop 'emtpy list'
        else
            this%currLink => this%firstLink
            nextLink => this%currLink%getNextLink()
            deallocate(this%currLink)
            this%currLink => nextLink
            prevLink => null()
            call this%currLink%setPrevLink(prevLink)
            this%firstLink => this%currLink
        end if
        prevLink => null()
        nextLink => null()
        
        call this%decrementSizeOfList()
        
    end subroutine removeFirstDoublyLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List�̖����v�f���폜�DcurrLink�͈�O��
    subroutine removeLastDoublyLink(this)
        implicit none
        class(DoublyList),intent(inout) :: this
        
        class(DoublyLink),pointer :: nextLink
        class(DoublyLink),pointer :: prevLink
        
        if( this%isEmpty() )then
            stop 'empty list'
        else
            this%currLink => this%lastLink
            prevLink => this%lastLink%getPrevLink()
            deallocate(this%lastLink)
            this%lastLink => prevLink
            nextLink      => null()
            call this%lastLink%setNextLink(nextLink)
            this%currLink => this%LastLink
        end if
        
        call this%decrementSizeOfList()
        
    end subroutine removeLastDoublyLink
    !-----------------------------------------------------------------------------------------!

    !-----------------------------------------------------------------------------------------!
    !List����ɂ���iList���̑S�v�f���폜�j
    subroutine clearList(this)
        implicit none
        class(DoublyList),intent(inout) :: this
        
        class(DoublyLink),pointer :: nextLink
        
        if( this%isEmpty() )then
            !List����Ȃ牽�����Ȃ�
        else
            this%currLink => this%firstLink
            do while(associated(this%currLink))
                nextLink=>this%currLink%getNextLink()
                deallocate(this%currLink)
                this%currLink  => nextLink
            end do
        end if
        this%firstLink => null()
        this%lastLink  => null()
             nextLink  => null()
        
        this%numberOfItem = 0
        
    end subroutine clearList
    !-----------------------------------------------------------------------------------------!
    
    !-----------------------------------------------------------------------------------------!
    !List�̗v�f���擾
    !-----------------------------------------------------------------------------------------!
    !List�̌��݈ʒu�̗v�f���擾
    function getCurrentValue(this) result(value)
        implicit none
        class(DoublyList) :: this
        
        class(*),pointer :: value
        
        if(.not.this%isEmpty())then
            value=>this%currLink%getValue()
        end if
    end function getCurrentValue
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List���̐擪����w�肵���ԍ��ڂ̗v�f���擾�DO(N)�̎��Ԃ��K�v
    function getValueOf(this,index) result(value)
        implicit none
        class(DoublyList) :: this
        integer,intent(in),value :: index
        integer :: i
        
        class(*),pointer :: value
        
        value => null()
        if(index > this%numberOfItem .or. this%isEmpty()) stop "index error"
        
        i=1
        this%currLink => this%firstLink
        do i=1,index-1
            this%currLink => this%currLink%getNextLink()
        end do
            value=>this%currLink%getValue()
            
    end function getValueOf
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List�̐擪�̗v�f���擾
    function getFirstValue(this) result(value)
        implicit none
        class(DoublyList) :: this
        
        class(*),pointer :: value
        
        if(.not.this%isEmpty())then
            value=>this%firstLink%getValue()
        end if
    end function getFirstValue
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List�̖����̗v�f���擾
    function getLastValue(this) result(value)
        implicit none
        class(DoublyList) :: this
        
        class(*),pointer :: value
        
        if(.not.this%isEmpty())then
            value=>this%lastLink%getValue()
        end if
    end function getLastValue
    !-----------------------------------------------------------------------------------------!

end module class_DoublyList